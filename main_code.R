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
       ggplot(aes(ysr, avg_rating)) + geom_point(alpha = 0.5,)
     
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
     
     # Trying loess
     
     trial_index <- createDataPartition(y = train_set$rating, times = 1, p = 0.10, 
                                        list = FALSE)
     trial_set <- train_set[trial_index,]
     nrow(trial_set)
     # Timing computation time
     # Start the clock!
     ptm <- proc.time()
     span <- 0.35
     fit_1 <- trial_set %>% 
       mutate(ysr = year(as_datetime(timestamp))-year) %>% 
       loess(rating ~ ysr, degree=1, span = span, data=.)
     
     # Stop the clock
     time <- proc.time() - ptm
     time[3]
     
     ysr_range <- seq(-2,93,1)
     
     ysr_bias <- train_set %>% 
       mutate(ysr = year(as_datetime(timestamp))-year, ysr_p = predict(fit_1,ysr)) %>%
       group_by(ysr) %>% summarise(ysr_b = mean(ysr_p))
     
     ysr_bias
     
     # Fill in the N/As
     
     ysr_bias$ysr_b[1] = ysr_bias$ysr_b[2]
     
     ysr_bias$ysr_b[96] = ysr_bias$ysr_b[95]
     
  # Average rating
    avg_rating <- mean(train_set$rating)
    l=5
    
  # Modeling movie bias with regularization
    
    movie_bias <- train_set %>%
      mutate(ysr = year(as_datetime(timestamp))-year) %>%
      left_join(ysr_bias,by = "ysr") %>%
      group_by(movieId) %>%
      summarize(mv_b = sum(rating - ysr_b)/(n()+l))
    
  # Modeling user bias with regularization
    
    user_bias <- train_set %>%
      mutate(ysr = year(as_datetime(timestamp))-year) %>%
      left_join(ysr_bias,by = "ysr") %>%
      left_join(movie_bias, by="movieId") %>%
      group_by(userId) %>%
      summarize(usr_b = sum(rating - mv_b - ysr_b)/(n()+l))
  
  # Modeling genre bias
    genre_bias <- train_set %>% 
      mutate(ysr = year(as_datetime(timestamp))-year) %>%
      left_join(ysr_bias,by = "ysr") %>%
      left_join(user_bias, by="userId") %>%
      left_join(movie_bias, by="movieId") %>%
      group_by(genres) %>% summarize(gnr_b = sum(rating - usr_b - mv_b - ysr_b)/(n()+l))
    

  

    # # Optimize lamda for regularization
    # lambdas <- seq(0, 20, 0.25)
    # 
    # rmses <- sapply(lambdas, function(l){
    #   
    #   # Modeling movie bias with regularization
    #   
    #   movie_bias <- train_set %>%
    #     mutate(ysr = year(as_datetime(timestamp))-year) %>%
    #     left_join(ysr_bias,by = "ysr") %>%
    #     group_by(movieId) %>%
    #     summarize(mv_b = sum(rating - ysr_b)/(n()+l))
    #   
    #   # Modeling user bias with regularization
    #   
    #   user_bias <- train_set %>%
    #     mutate(ysr = year(as_datetime(timestamp))-year) %>%
    #     left_join(ysr_bias,by = "ysr") %>%
    #     left_join(movie_bias, by="movieId") %>%
    #     group_by(userId) %>%
    #     summarize(usr_b = sum(rating - mv_b - ysr_b)/(n()+l))
    #   
    #   # Modeling genre bias
    #   genre_bias <- train_set %>% 
    #     mutate(ysr = year(as_datetime(timestamp))-year) %>%
    #     left_join(ysr_bias,by = "ysr") %>%
    #     left_join(user_bias, by="userId") %>%
    #     left_join(movie_bias, by="movieId") %>%
    #     group_by(genres) %>%
    #     summarize(gnr_b = sum(rating - usr_b - mv_b - ysr_b)/(n()+l))
    # 
    # 
    #   y_hat <-  test_set %>% 
    #     left_join(user_bias, by="userId") %>%
    #     left_join(movie_bias, by="movieId") %>%
    #     left_join(genre_bias, by="genres") %>% 
    #     mutate(ysr = year(as_datetime(timestamp))-year) %>%
    #     left_join(ysr_bias, by="ysr") %>%
    #     mutate(p_rating = ysr_b + usr_b + mv_b + gnr_b)
    # 
    #   return(RMSE(test_set$rating, y_hat$p_rating))
    # })
    # 
    # qplot(lambdas, rmses)

# Making final predictions
  
    
    y_hat <-  test_set %>% 
      left_join(user_bias, by="userId") %>%
      left_join(movie_bias, by="movieId") %>%
      left_join(genre_bias, by="genres") %>% 
      mutate(ysr = year(as_datetime(timestamp))-year) %>%
      left_join(ysr_bias, by="ysr") %>%
      mutate(p_rating = ysr_b + usr_b + mv_b + gnr_b)

    summary(y_hat)

    # Make sure predictions are within rating range
    
    y_hat$p_rating[y_hat$p_rating<0] = 0
    
    y_hat$p_rating[y_hat$p_rating>5] = 5

    y_hat %>% select(p_rating) %>% summary 
 
  # Inspect errors
    
    error_hist <- y_hat %>% mutate(error = rating - p_rating) %>% ggplot(aes(error)) + geom_histogram()
    error_hist
    y_hat %>% mutate(error = abs(rating - p_rating)) %>% group_by(title, movieId) %>% 
      summarise(avg_error= mean(error),no_reviews = n(), mv_b = first(mv_b)) %>%
       arrange(desc(avg_error)) %>% top_n(10)

  train_set %>% filter(userId==4043) %>% ggplot(aes(rating)) + geom_histogram()
# Calculate RMSE
  
  RMSE <- function(ratings, p_hat){
    sqrt(mean((ratings - p_hat)^2))
  }
  
  RMSE(test_set$rating, y_hat$p_rating)

 
  