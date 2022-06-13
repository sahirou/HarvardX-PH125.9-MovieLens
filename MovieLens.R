

################################################################################
# Create edx set, validation set (final hold-out test set)
################################################################################

cat('\n > Data pre-processing : running EDX code chunc \n')

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")
options(scipen=1000)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(
  text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
  col.names = c("userId", "movieId", "rating", "timestamp")
)

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")



# if using R 3.6 or earlier:
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 6))) {
  print("R 3.6 or earlier")
  
  movies <- as.data.frame(movies) %>% 
    mutate(
      movieId = as.numeric(levels(movieId))[movieId],
      title = as.character(title),
      genres = as.character(genres)
    )
}


# if using R 4.0 or later:
if(as.integer(R.version$major) >= 4) {
  print("R 4.0 or later")
  
  movies <- as.data.frame(movies) %>% 
    mutate(
      movieId = as.numeric(movieId),
      title = as.character(title),
      genres = as.character(genres)
    )
}


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
# if using R 3.5 or earlier, use `set.seed(1)`
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(1)
} else {
  print("not using R 3.5 or earlier")
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
}


test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)






################################################################################
# Useful functions
################################################################################

## Print tabular data
print_tab_data <- function(df) {
  tab <- df %>%
    kable(longtable = T) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      position = "center",
      font_size = 10,
      full_width = FALSE
    )
  
  return(tab)
}


## The RMSE function that will be used
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}





################################################################################
# Further data preparation
################################################################################


## Glimpse edx data set
initial_edx_training_set_piece <- edx %>% head()
# glimpse(initial_edx_training_set_piece)

## Look for NAs in both training and validation sets
check_na_message <- case_when(
  any(is.na(edx)) & any(is.na(validation)) ~ "There is missing values (NAs) in both traning and validation sets.",
  any(is.na(edx)) & !any(is.na(validation)) ~ "There is missing values (NAs) in traning set.",
  !any(is.na(edx)) & any(is.na(validation)) ~ "There is missing values (NAs) in validation set.",
  !any(is.na(edx)) & !any(is.na(validation)) ~ "There is no missing values (NAs) in both traning and validation sets.",
  TRUE ~ "-"
)

## Look for 0 stat ratings in  the training set
zero_star_rating_message <- case_when(
  edx %>% filter(rating == 0) %>% nrow() == 0 ~ "No zero 0-star rating was given in the edx training set.",
  TRUE ~ paste0("We have ", edx %>% filter(rating == 0) %>% nrow(), "0-star rating(s) in the training set")
)



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract year of release from title field 
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Further data preparation \n')

cat('\n > Extract year of release from title field \n')

## training set
edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(
    title,
    c("movieTitle", "movieReleaseYear"),
    regex = "^(.*) \\(([0-9]+)\\)$",
    remove = FALSE
  ) %>%
  mutate(
    movieTitle = ifelse(is.na(movieTitle), title, movieTitle),
    movieReleaseYear = as.integer(str_sub(string = str_trim(movieReleaseYear), start = 1, end = 4))
  ) %>%
  select(-c(title))

# edx %>% select(movieReleaseYear) %>% distinct() %>% .$movieReleaseYear
# any(is.na(edx))


## validation set
validation <- validation %>%
  mutate(title = str_trim(title)) %>%
  extract(
    title,
    c("movieTitle", "movieReleaseYear"),
    regex = "^(.*) \\(([0-9]+)\\)$",
    remove = FALSE
  ) %>%
  mutate(
    movieTitle = ifelse(is.na(movieTitle), title, movieTitle),
    movieReleaseYear = as.integer(str_sub(string = str_trim(movieReleaseYear), start = 1, end = 4))
  ) %>%
  select(-c(title))

# validation %>% select(movieReleaseYear) %>% distinct() %>% .$movieReleaseYear
# any(is.na(validation))



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract pipe-separated values from genres 
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Extract pipe-separated values from genres \n')

## training set
edx <- edx %>%
  mutate(
    genre = fct_explicit_na(
      genres,
      na_level = "(no genres listed)"
    )
  ) %>%
  separate_rows(genre, sep = "\\|")

# edx %>% select(genre) %>% distinct() %>% .$genre
# any(is.na(edx))


## validation set
validation <- validation %>%
  mutate(
    genre = fct_explicit_na(
      genres,
      na_level = "(no genres listed)"
    )
  ) %>%
  separate_rows(genre, sep = "\\|")

# validation %>% select(genre) %>% distinct() %>% .$genre
# any(is.na(validation))



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract year and month from the timestamp field
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Extract year and month from the timestamp field \n')

## training set
edx <- edx %>%
  mutate(
    date = as.POSIXct(timestamp, origin="1970-01-01"),
    ratingYear = as.integer(format(date, "%Y")),
    ratingMonth = as.integer(format(date, "%m"))
  ) %>%
  select(
    movieId, movieTitle, movieReleaseYear, genre,
    userId, rating, ratingMonth, ratingYear
  )

# any(is.na(edx))


## validation set
validation <- validation %>%
  mutate(
    date = as.POSIXct(timestamp, origin="1970-01-01"),
    ratingYear = as.integer(format(date, "%Y")),
    ratingMonth = as.integer(format(date, "%m"))
  ) %>%
  select(
    movieId, movieTitle, movieReleaseYear, genre,
    userId, rating, ratingMonth, ratingYear
  )

# any(is.na(validation))





################################################################################
# Exploratory Data Analysis
################################################################################

cat('\n > Exploratory Data Analysis \n')

#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Overview \n')

## Half star rating in the training set
half_star_rating <- edx %>%
  select(rating) %>%
  mutate(type = if_else(rating - trunc(rating) == 0, "full", "half")) %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(type, count) %>%
  mutate(
    half_star_rating = half / (half + full),
    full_star_rating = full / (half + full)
  ) %>%
  select(half_star_rating, full_star_rating)


## Positive rating in the training set
positive_rating <- edx %>%
  select(rating) %>%
  mutate(type = if_else(rating > 3 , "positive", "nagative")) %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(type, count) %>%
  mutate(
    positive_rating = positive / (positive + nagative),
    negative_rating = nagative / (positive + nagative)
  ) %>%
  select(positive_rating, negative_rating)


## Global rating distribution
overview_ratings_distribution_gg <- edx %>%
  ggplot(aes(rating)) +
  geom_bar() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Ratings") +
  ylab("Number of ratings") +
  ggtitle("Global ratings distribution") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.title.x = element_text(color="#010101"),
    legend.position = "none"
  ) 



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Rating distribution per movie genre
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Rating distribution per movie genre \n')

## Most rated genres
most_rated_genres <- edx %>% 
  filter(genre != "(no genres listed)") %>% 
  group_by(genre) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

## Rating distribution per movie genre
ratings_distibution_per_genre_gg <- edx %>%
  ggplot(aes(genre)) +
  geom_bar() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Genre") +
  ylab("Number of ratings") +
  ggtitle("Distribution of ratings by movie genre") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.text.x = element_text(color="#010101", angle = 90, hjust = 1),
    legend.position = "none"
  )


#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Movie ratings by genre
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Movie ratings by genre \n')

## Better rated genres
better_rated_genres <- edx %>% 
  filter(genre != "(no genres listed)") %>% 
  group_by(genre) %>%
  summarise(rating_mean = mean(rating)) %>%
  ungroup() %>%
  arrange(desc(rating_mean))

rating_by_genre_gg <- edx %>% 
  ggplot(aes(genre, rating)) + 
  geom_boxplot(varwidth = TRUE) +
  xlab("Genre") +
  ylab("Rating") +
  ggtitle("Movie ratings per genre") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.text.x = element_text(color="#010101", angle = 90, hjust = 1),
    legend.position = "none"
  )


#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Ratings by movie
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Ratings by movie \n')

## Ratings distribution per movie
ratings_distibution_per_movie_gg <- edx %>%
  ggplot(aes(movieId)) +
  geom_histogram(bins=500) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("movieId") +
  ylab("Number of ratings") +
  ggtitle("Ratings distribution per movie") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.title.x = element_text(color="#010101"),
    legend.position = "none"
  )

## Top 50 rated movies
top_50_rated_movies_list <- edx %>%
  group_by(movieTitle) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  head(50) %>%
  .$movieTitle

top_50_rated_movies <- edx %>%
  select(movieTitle) %>%
  mutate(top50 = if_else(movieTitle %in% top_50_rated_movies_list, "Yes", "No")) %>%
  group_by(top50) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(top50, count) %>%
  mutate(
    top50 = Yes / (Yes + No),
    others = No / (Yes + No)
  ) %>%
  select(top50, others) 

## Top 50 rated movies chart
top_50_rated_movies_gg <- edx %>%
  group_by(movieTitle) %>%
  summarise(occurrence = n()) %>%
  ungroup() %>% 
  arrange(desc(occurrence)) %>%
  head(50) %>% 
  ggplot(aes(movieTitle, occurrence)) +
  geom_col() +
  # coord_flip() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Title") +
  ylab("Number of ratings") +
  ggtitle("Top 50 rated movies") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.text.x = element_text(color="#010101", angle = 90, hjust = 1, size = 7),
    legend.position = "none"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Ratings distribution per use
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Ratings distribution per user \n')

ratings_distibution_per_user_gg <- edx %>%
  ggplot(aes(userId)) +
  geom_histogram(bins=500) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("userId") +
  ylab("Number of ratings") +
  ggtitle("Ratings distribution per user") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.title.x = element_text(color="#010101"),
    legend.position = "none"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Ratings distribution per Month/Year of ratings 
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Ratings distribution per Month/Year of ratings \n')

ratings_distribution_per_month_gg <- edx %>%
  ggplot(aes(as.factor(ratingMonth))) +
  geom_bar() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Month of rating") +
  ylab("Number of ratings") +
  ggtitle("Distribution of ratings through months") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.title.x = element_text(color="#010101"),
    legend.position = "none"
  )

ratings_distribution_per_year_gg <- edx %>%
  ggplot(aes(as.factor(ratingYear))) +
  geom_bar() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Year of rating") +
  ylab("Number of ratings") +
  ggtitle("Distribution of ratings through years") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.title.x = element_text(color="#010101"),
    legend.position = "none"
  )




################################################################################
# Model Building and Evaluation 
################################################################################

cat('\n > Model Building and Evaluation \n')

## Global results dataframe that contains all models RMSE results
results <- tibble()


#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Naive Mean-Baseline Model
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Naive Mean-Baseline Model \n')

## Average of all ratings
mu_hat <- mean(edx$rating)

## RMSE on the validation set
rmse_mean_baseline_model_result <- RMSE(validation$rating, mu_hat)

## Update results dataframe
results <- results %>% 
  bind_rows(tibble(
    Model="Naive Mean-Baseline Model", 
    RMSE=rmse_mean_baseline_model_result
  ))



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Mean + Movie Effect Model
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Mean + Movie Effect Model \n')

## Average of all ratings
mu_hat <- mean(edx$rating)

## Average by movie (movies biais)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat)) %>%
  ungroup()

## Compute the predicted ratings on validation set
pred_ratings_mean_movie_model <- validation %>%
  left_join(movie_avgs, by=c('movieId' = 'movieId')) %>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)

## RMSE on the validation set
rmse_mean_movie_model_result <- RMSE(
  validation$rating, 
  pred_ratings_mean_movie_model
)

## Update results dataframe
results <- results %>% 
  bind_rows(tibble(
    Model="Mean + Movie Effect Model", 
    RMSE=rmse_mean_movie_model_result
  ))



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Mean + Movie + User Effects Model  
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Mean + Movie + User Effects Model \n')

## Average of all ratings
mu_hat <- mean(edx$rating)

## Average by movie (movies biais)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat)) %>%
  ungroup()
 
## Average by user (users biais)
user_avgs <- edx %>%
  left_join(movie_avgs, by=c('movieId' = 'movieId')) %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i)) %>%
  ungroup()

## Compute the predicted ratings on validation dataset
pred_ratings_mean_movie_user_model <- validation %>%
  left_join(movie_avgs, by=c('movieId' = 'movieId')) %>%
  left_join(user_avgs, by=c('userId' = 'userId')) %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

## RMSE on the validation set
rmse_mean_movie_user_model_result <- RMSE(
  validation$rating, 
  pred_ratings_mean_movie_user_model
)

## Update results dataframe
results <- results %>% 
  bind_rows(tibble(
    Model="Mean + Movie + User Effects Model", 
    RMSE=rmse_mean_movie_user_model_result
  ))



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Mean + Movie + User + Genre Effetcs Model  
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Mean + Movie + User + Genre Effetcs Model  \n')

## Average of all ratings
mu_hat <- mean(edx$rating)

## Average by movie (movies biais)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat)) %>%
  ungroup()

## Average by user (users biais)
user_avgs <- edx %>%
  left_join(movie_avgs, by=c('movieId' = 'movieId')) %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i)) %>%
  ungroup()

## Avarage by gender (movies biais)
genre_avgs <- edx %>%
  left_join(movie_avgs, by=c('movieId' = 'movieId')) %>%
  left_join(user_avgs, by=c('userId' = 'userId')) %>%
  group_by(genre) %>%
  summarize(b_u_g = mean(rating - mu_hat - b_i - b_u)) %>%
  ungroup()

## Compute the predicted ratings on validation dataset
pred_ratings_mean_movie_user_genre_model <- validation %>%
  left_join(movie_avgs, by=c('movieId' = 'movieId')) %>%
  left_join(user_avgs, by=c('userId' = 'userId')) %>%
  left_join(genre_avgs, by=c('genre' = 'genre')) %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
  pull(pred)

## RMSE on the validation set
rmse_mean_movie_user_model_result <- RMSE(
  validation$rating, 
  pred_ratings_mean_movie_user_genre_model
)

## Update results dataframe
results <- results %>% 
  bind_rows(tibble(
    Model="Mean + Movie + User + Genre Effetcs Model", 
    RMSE=rmse_mean_movie_user_model_result
  ))





################################################################################
# Regularization
################################################################################

cat('\n > Regularization \n')

#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Regularized Mean + Movie Effect Model   
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Regularized Mean + Movie Effect Model \n')

## Average of all movies
mu_hat <- mean(edx$rating)

## Lambda values
lambdas <- seq(0, 15, 0.1)

## Compute Predicted ratings on validation set for different values of lambda
rmses <- sapply(lambdas, function(lambda) {
  ## Average by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda)) %>%
    ungroup()
  
  ## Predicted ratings on validation set
  predicted_ratings <- validation %>%
    left_join(b_i, by=c('movieId' = 'movieId')) %>%
    mutate(pred = mu_hat + b_i) %>%
    pull(pred)
  
  ## RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

## Plot results
tibble(RMSE = rmses, lambdas = lambdas) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  labs(
    title = "RMSEs vs Lambdas for Regularized Mean + Movie Effect Model",
    y = "RMSEs",
    x = "Lambdas"
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.text.x = element_text(color="#010101"),
    legend.position = "none"
  ) -> regularized_mean_movie_model_gg

## Find lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

## Compute the RMSE on the validation set
rmse_regularized_mean_movie_model_result <- min(rmses)

## Update results dataframe
results <- results %>% 
  bind_rows(tibble(
    Model="Regularized Mean + Movie Effect Model", 
    RMSE=rmse_regularized_mean_movie_model_result
  ))



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Regularized Mean + Movie + User Effects Model   
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Regularized Mean + Movie + User Effects Model \n')

## Average of all movies
mu_hat <- mean(edx$rating)

## Lambda values
lambdas <- seq(0, 15, 0.1)

## Compute Predicted ratings on validation set for different values of lambda
rmses <- sapply(lambdas, function(lambda) {
  ## Calculate the average by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda)) %>%
    ungroup()
  
  ## Average by user
  b_u <- edx %>%
    left_join(b_i, by=c('movieId' = 'movieId')) %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda)) %>%
    ungroup()
  
  ## Predicted ratings on validation set
  predicted_ratings <- validation %>%
    left_join(b_i, by=c('movieId' = 'movieId')) %>%
    left_join(b_u, by=c('userId' = 'userId')) %>%
    mutate(pred = mu_hat + b_i + b_u) %>%
    pull(pred)
    
  ## RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

## Plot the results
tibble(RMSE = rmses, lambdas = lambdas) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  labs(
    title = "RMSEs vs Lambdas for Regularized Mean + Movie + User Effects Model",
    y = "RMSEs",
    x = "Lambdas"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.text.x = element_text(color="#010101"),
    legend.position = "none"
  ) -> regularized_mean_movie_user_model_gg

## Find lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

## Compute the RMSE on the validation set
rmse_regularized_mean_movie_user_model_result <- min(rmses)

## Update results dataframe
results <- results %>% 
  bind_rows(tibble(
    Model="Regularized Mean + Movie + User Effects Model", 
    RMSE=rmse_regularized_mean_movie_user_model_result
  ))



#+++++++++++++++++++++++++++++++++++++++++++++++++++
# Regularized Mean + Movie + User + Genre Effetcs Model  
#+++++++++++++++++++++++++++++++++++++++++++++++++++

cat('\n > Regularized Mean + Movie + User + Genre Effetcs Model \n')

## Average of all movies
mu_hat <- mean(edx$rating)

## Lambda values
lambdas <- seq(0, 15, 0.1)

## Compute Predicted ratings on validation set for different values of lambda
rmses <- sapply(lambdas, function(lambda) {
  ## Average by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda)) %>%
    ungroup()
  
  ## Calculate the average by user
  b_u <- edx %>%
    left_join(b_i, by=c('movieId' = 'movieId')) %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda)) %>%
    ungroup()
  
  ## Calculate the average by genre
  b_u_g <- edx %>%
    left_join(b_i, by=c('movieId' = 'movieId')) %>%
    left_join(b_u, by=c('userId' = 'userId')) %>%
    group_by(genre) %>%
    summarize(b_u_g = sum(rating - b_i - mu_hat - b_u) / (n() + lambda)) %>%
    ungroup()
  
  ## Compute the predicted ratings on validation dataset
  predicted_ratings <- validation %>%
    left_join(b_i, by=c('movieId' = 'movieId')) %>%
    left_join(b_u, by=c('userId' = 'userId')) %>%
    left_join(b_u_g, by=c('genre' = 'genre')) %>%
    mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
    pull(pred)
  
  ## RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

# Plot the results
tibble(RMSE = rmses, lambdas = lambdas) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  labs(
    title = "RMSEs vs Lambdas for Regularized Mean + Movie + User + Genre Effetcs Model",
    y = "RMSEs",
    x = "Lambdas"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size=rel(1.0)),
    axis.title.y = element_text(color="#010101"),
    axis.text.x = element_text(color="#010101"),
    legend.position = "none"
  ) -> regularized_mean_movie_user_genre_model_gg

## Find the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

## Compute the RMSE on the validation set
rmse_regularized_mean_movie_user_genre_model_result <- min(rmses)


## Update results dataframe
results <- results %>% 
  bind_rows(tibble(
    Model="Regularized Mean + Movie + User + Genre Effetcs Model", 
    RMSE=rmse_regularized_mean_movie_user_genre_model_result
  ))











