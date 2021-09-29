# Load libraries
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggridges)

# Load edx data frame
load("movielens_edx.Rda")

# Quiz

q2_1 <- nrow(edx[edx$rating == 0])
q2_2 <- nrow(edx[edx$rating == 3])
q3 <- length(unique(edx$movieId))
q4 <- length(unique(edx$userId))
q5 <- edx %>% filter(str_detect(genres,"Romance"))
q6 <- edx %>% group_by(rating) %>% summarise(no = n()) %>% arrange(by= desc(no))
q7 <- edx %>% group_by(rating) %>% summarise(no_ratings = n()) %>% arrange(by= desc(no_ratings))
####################################

# Data Cleaning 
  #Completeness Check
    #Check for NAs and blanks
      na <- edx[rowSums(is.na(edx)) > 0,] # none
    # Do movies with the same id have different titles
      unq_mID <- length(unique(edx$movieId)) 
      unq_title <-length(unique(edx$title)) 
      diff <- unq_mID - unq_title
      # 1 more ID than there is titles, suggesting there is a movie with two IDs
      ax <- edx %>% group_by(title) %>% summarise(diff = diff(movieId)) %>% filter(diff > 0)
      wotw <- edx %>% filter(title == ax$title[1]) %>% group_by(movieId) %>% 
      summarise(no_ratings = n(),title[1])
      wotw
      #variable wotw shows that there are two movieIDs for War of the Worlds (2005)
    # The title column contains year data. Check this is true for every title.
    yr_pattern <- "\\(\\d{4}\\)"
    title_check <- edx %>% group_by(title) %>% filter(str_detect(title,yr_pattern,negate = TRUE))
    # title check is empty proving that all titles have 
    
  #Function for correcting War of the Worlds (2005) ID
  
  WotW_corr <- function(df) {
    df %>% mutate(movieId = replace(movieId, movieId == 64997, 34048))
  }
  #Function extracting movie year
  ext_year <- function(df) {
    df %>% mutate(year = str_sub(str_extract(title,yr_pattern), start = 2L, end = -2L)) %>% 
      mutate(year = as.integer(year))
  }
 
  # Correct WotW and extract year
  edx_y <- WotW_corr(edx)
  edx_y <- ext_year(edx_y)

# Date Exploration
  # Ratings
  rtngs <- edx_y %>% group_by(rating) %>%
    ggplot(aes(rating)) + geom_bar()
  rtngs
  # Reviews per user 
  usr_rev_plot <- edx_y %>% group_by(userId) %>% summarise(no_reviews = n()) %>% 
    ggplot(aes(no_reviews)) + geom_histogram(bin=2) + xlim(0,1000) 
  usr_rev_plot
  usr_rev_sum <- edx_y %>% group_by(userId) %>% summarise(no_reviews = n()) %>%
    select(no_reviews) %>% summary()
  usr_rev_sum
  
  # Reviews per movie
  
  mv_rev_plot <- edx_y %>% group_by(movieId) %>% summarise(no_reviews = n()) %>% 
    ggplot(aes(no_reviews)) + geom_histogram(bin=2) + xlim(0,1000) 
  mv_rev_plot
  mv_rev_sum <- edx_y %>% group_by(movieId) %>% summarise(no_reviews = n()) %>%
    select(no_reviews) %>% summary()
  usr_rev_sum
  
  # Relationship rating - time of day
  
    time_anlys <- edx_y %>% mutate(hour = hour(as_datetime(timestamp))) %>% group_by(hour) %>%
    summarise(avg = mean(rating),sd = sd(rating)) %>% arrange(by=desc(avg)) %>% 
    ggplot(., aes(x=hour)) + 
    geom_line(aes(y = avg), color = "red") + 
    geom_line(aes(y = sd), color="steelblue")
  
  # Relationship rating - genre
    genres_rts <-  edx_y %>% group_by(genres) %>% 
      summarise(avg_rating=mean(rating), no_reviews = n(), no_movies = length(unique(movieId)))
    # lowest avg rating by genre
    genres_rts %>% arrange(by=avg_rating) %>% slice(1:10)

    # highest avg rating by genre
    genres_rts %>% arrange(by=desc(avg_rating)) %>% slice(1:10)
  
  # Relationship rating - year
    yr_rts <-  edx_y %>% group_by(year) %>% 
      summarise(avg_rating=mean(rating), no_reviews = n(), no_movies = length(unique(movieId))) 
    yr_rts
  
  # Relationship rating - years since release
     edx_y %>% mutate(ysr = year(as_datetime(timestamp))-year) %>% 
       group_by(ysr) %>% summarise(avg_rating = mean(rating)) %>% 
       ggplot(aes(ysr, rating)) + geom_point(alpha = 0.5,)
     
     # Can the function be modeled by knn?
     fit_ysr_knn <- train_set %>% mutate(ysr = year(as_datetime(timestamp))-year) %>% 
       train(rating ~ ysr, data = ., method="knn")
     ysr <- seq(1,80,1)
     prediction <- predict(fit_ysr_knn,ysr)
     
     as_data_frame(prediction)%>% mutate(ysr=ysr) %>% 
       ggplot(aes(ysr,prediction)) + geom_line()
     
# Developing a model
  
  # Divide training and testing
     test_index <- createDataPartition(y = edx_y$rating, times = 1, p = 0.2, 
                                       list = FALSE)
     train_set <- edx_y[-test_index,]
     test_set <- edx_y[test_index,]
     
     test_set <- test_set %>% 
       semi_join(train_set, by = "movieId") %>%
       semi_join(train_set, by = "userId")
  
  # Average rating
    avg_rating <- mean(train_set$rating)
    
  # Modeling user bias
    
    user_bias <- train_set %>% 
      group_by(userId) %>%
      summarise(usr_b = mean(rating - avg_rating))
    
  # Modeling movie bias  
     
    movie_bias <- train_set %>% 
      group_by(movieId) %>%
      summarise(mv_b = mean(rating - avg_rating))
  
  # Modeling genre bias
    genre_bias <- train_set %>% 
      group_by(genres) %>%
      summarise(gnr_b = mean(rating - avg_rating))
    
  # Modeling years since release bias
    
    fit_ysr_knn <- train_set[1:8000,] %>% 
      mutate(ysr = year(as_datetime(timestamp))-year) %>%
      train(rating~ysr, data =., method = "knn")
    fit_ysr_knn
  ysr_range <- seq(1,80,1)
    y_hat_ysr <- predict(fit_ysr_knn,ysr_range)
    gg
    ysr_bias <- as_data_frame(y_hat_ysr)%>% mutate(ysr= ysr_range, ysr_b = y_hat_ysr - avg_rating)
    
  # Regularisation
    # lambdas <- seq(0, 10, 0.25)
    # 
    # rmses <- sapply(lambdas, function(l){
    #   
    #   
    #   b_i <- train_set %>% 
    #     group_by(movieId) %>%
    #     summarize(b_i = sum(rating - )/(n()+l))
    #   
    #   b_u <- train_set %>% 
    #     left_join(b_i, by="movieId") %>%
    #     group_by(userId) %>%
    #     summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    #   
    #   predicted_ratings <- 
    #     test_set %>% 
    #     left_join(b_i, by = "movieId") %>%
    #     left_join(b_u, by = "userId") %>%
    #     mutate(pred = mu + b_i + b_u) %>%
    #     pull(pred)
    #   
    #   return(RMSE(predicted_ratings, test_set$rating))
    # })
    
# Making predictions
    
    # 
  
# Calculate RMSE
  
  RMSE <- function(ratings, p_hat){
    sqrt(mean((ratings - p_hat)^2))
  }
  

 
 
 
  