# load the libraries

options(round = 5)
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", 
                                              repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", 
                                      repos = "http://cran.us.r-project.org")
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#------------------------------------------------------------------------------------
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, removed)
#------------------------------------------------------------------------------------

#summary of the data set
summary(edx)

# unique users and movies
edx %>% summarise(unique_users = n_distinct(userId),
                  unique_movies = n_distinct(movieId))

range(edx$rating)
# range of the ratings
range(as_datetime(edx$timestamp))
#titles
head(edx$title)
#genres
head(edx$genres)

## Exploration of data

### Rating distribution

edx %>% group_by(rating) %>%
  ggplot(aes(rating)) + geom_histogram(fill=4, bins=30)

### User, Movie distribution
p1 <- edx %>% dplyr::count(movieId) %>% 
  ggplot(aes(n)) + geom_histogram(bins = 20, color=3, fill=4) +
  scale_x_log10() + ggtitle("Movies")
p2 <- edx %>% dplyr::count(userId) %>%
  ggplot(aes(n)) + geom_histogram(bins = 20, color=3, fill=4) +
  scale_x_log10() + ggtitle("Users")
gridExtra::grid.arrange(p1,p2, ncol=2)

#sparisity of the data set
users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users") %>%
  abline(h=0:100+0.2, v=0:100+0.2, col = "grey")

# distribution of ratings, by user and movies, using ranking
#lowest and highest 20 ranks are colored cyan
temp <- edx %>% group_by(userId) %>% 
  summarize(user_rating = n(), mean_r_user = mean(rating))
temp <- temp %>% mutate(rank = rank(-mean_r_user) )
p1 <- temp %>%
  ggplot(aes(user_rating, mean_r_user)) + 
  geom_point(alpha=0.2) + 
  geom_point(data = filter(temp, rank <= 20 | rank >= 69856), col=13)

tempm <- edx %>% group_by(movieId) %>%
  summarize(movie_rating = n(), mean_r_movie = mean(rating))
tempm <- tempm %>% mutate(rank = rank(-mean_r_movie))
p2 <- tempm %>% ggplot(aes(movie_rating, mean_r_movie)) + 
  geom_point(alpha=0.2) +
  geom_point(data = filter(tempm, rank <= 20 | rank >= 10655), col=13)
gridExtra::grid.arrange(p1,p2,ncol=2)

### Timestamp 

# distribution by the month and week on the timestamp
edx_m <- edx %>% mutate(date_stamp = as_datetime(timestamp))
p1 <- edx_m %>% mutate(date_month = round_date(date_stamp, unit = "month")) %>%
  group_by(date_month) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date_month, rating)) +
  geom_point() +
  geom_smooth()
p2 <- edx_m %>% mutate(date_week = round_date(date_stamp, unit = "week")) %>%
  group_by(date_week) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date_week, rating)) +
  geom_point() +
  geom_smooth()
gridExtra::grid.arrange(p1,p2, ncol=2)

# the str_sub function is used to get the last 6 characters from the title and the str_remove_all is used to remove the () from the extracted string.
edx_m <- edx %>% 
  mutate( movie_year = as.numeric(str_remove_all(str_sub(title, -6),"[^[:alnum:]]")),
          date_stamp = as_datetime(timestamp),
          year_t = year(date_stamp))
#age of movie is the year of watching minus the year of the make of a movie.
edx_m <- edx_m %>% mutate(age_of_movie = as.factor(year_t - movie_year))

#Visualizing the Age of movie effect.

p1 <- edx_m %>% group_by(movie_year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(movie_year, rating)) +
  geom_point() +
  geom_smooth()
p2 <- edx_m %>% mutate(age_of_movie = year_t - movie_year) %>%
  group_by(age_of_movie) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(age_of_movie, rating)) +
  geom_point() +
  geom_smooth()
gridExtra::grid.arrange(p1,p2, ncol=2)

# adding the total_ratings to the data set

edx_m <- edx %>% group_by(movieId) %>% mutate(total_ratings = n())
edx_m <- data.frame(edx_m)
#visualizing the total_ratings effect
edx_m %>% group_by(movieId) %>% 
  mutate(total_ratings = n()) %>%
  group_by(total_ratings) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(total_ratings, mean_rating)) +
  geom_point() +
  geom_smooth()

temp <- edx %>% group_by(genres) %>% 
  summarize(num_rating = n(), mean_r = mean(rating))
temp <- temp %>% mutate(rank = rank(-mean_r) )

temp %>% filter( num_rating >= 1000) %>% 
  ggplot(aes(num_rating/1000, mean_r)) + 
  geom_point(alpha=0.2) + 
  geom_point(data = filter(temp, rank <= 10), col=13)
temp %>% arrange(desc(num_rating))

#-----------------------------------------------------------
#this does take some time to complete!
#using separate_rows with | as a separator and summarising the count and the means.

#genres_list <- edx %>% separate_rows(genres, sep = "\\|") %>%
#  group_by(genres) %>% 
#  summarize(count = n(), mean_g = mean(rating)) %>%
#  arrange(desc(count))
#used for initializing faster
#write.csv(genres_list, "genres_list.csv")
#-----------------------------------------------------------


#genres_list csv uploaded along with the files
genres_list <- read.csv("genres_list.csv")
genres_list

# distribution of the genres and ratings
p1 <- genres_list %>% 
  ggplot(aes(reorder(genres,count),count)) +
  geom_bar(stat="identity", fill= 4) +
  coord_flip() +
  theme(axis.text.y = element_text(size=8)) +
  ylab("Number of ratings") + xlab('')
p2 <- genres_list %>%
  ggplot(aes(reorder(genres,mean_g),mean_g)) +
  geom_bar(stat="identity", fill=4) +
  coord_flip() +
  theme(axis.text.y = element_text(size=8)) + 
  ylab("Mean of ratings") + xlab('')
gridExtra::grid.arrange(p1,p2,ncol=2)

#range of the mean ratings by genres
range(genres_list$mean_g)

# distribution by the genres (as listed in the dataset)
edx_m %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 42000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point(color=7) +
  geom_errorbar(color="red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(edx_m, genres_list, p1, p2, temp)

### Similarity of movie, user ratings

edx_sub <- edx %>% select(userId, movieId, rating)
edx_sub <- as.data.frame(edx_sub)
#using the recommenderlab package
#Create realRatingMatrix
rrm <- as(edx_sub, "realRatingMatrix")
#to normalize the matrix
class(rrm)

#normalize the matrix
rrm_n <- normalize(rrm)

p1 <- image(rrm[1:100,1:100], main = "Raw Rating")
p2 <- image(rrm_n[1:100,1:100], main="Normalized Rating")
gridExtra::grid.arrange(p1,p2,ncol=2)

#similarity ratings
similar_user <- similarity(rrm[1:200], method = "pearson", which = "users")
similar_movies <- similarity(rrm[,1:200], method = "pearson", which = "items")
par(mfrow=c(1,2))
image(as.matrix(similar_user), main= "User Similarity")
image(as.matrix(similar_movies), main= "Movie Similarity")

### Hierarchical Clustering

#matrix of top 50 movies and users with more than 25 ratings in that.
temp_m <- edx %>% group_by(movieId) %>% summarize(n=n(), title=first(title))%>%
  top_n(50, n) %>% pull(movieId)
x <- edx %>% filter(movieId %in% temp_m) %>% group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>% select(title, userId, rating) %>%
  spread(userId,rating)
#adding the rownames and the colnames
row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()
x <- sweep(x,2,colMeans(x, na.rm = TRUE))
x <- sweep(x,1,rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

#calculate the distance between each of the movies and the dendrogram
h <- dist(x) %>% hclust()
plot(h, cex=0.65, main="", xlab="")

# using cutree to segregate the groups
groups <- cutree(h, k=10)
cbind(names(groups)[groups==2],names(groups)[groups==7])
#generate the heatmap of the first 25 entries - system limitation
x_sub <- x[1:25,1:25]
heatmap(x_sub, col = RColorBrewer::brewer.pal(8,"RdBu"))

# cleanup
rm(edx_sub, groups, h, p1, p2, row_names, rrm, rrm_n, similar_movies, similar_user, temp_m, x, x_sub )



## Prospective Model Features

set.seed(1977, sample.kind = "Rounding")

#ColdStart Fix
edx_m <- edx %>% group_by(movieId) %>% filter(n() >= 25)
edx_m <- edx_m %>% 
  mutate( movie_year = as.numeric(str_remove_all(str_sub(title, -6),"[^[:alnum:]]")),
          date_stamp = as_datetime(timestamp), year_t = year(date_stamp))
edx_m <- edx_m %>% mutate(age_of_movie = as.factor(year_t - movie_year))
edx_m <- edx_m %>% mutate(date_month = round_date(date_stamp, unit = "month"))
edx_m <- edx_m %>% group_by(movieId) %>%  mutate(total_ratings = n())
edx_m <- edx_m %>% select(userId, movieId, rating, genres, age_of_movie, date_month, total_ratings)
edx_m <- as.data.frame(edx_m)

mu <- mean(edx_m$rating)
mu

#running a comparison on all the features and 
#visualizing how each feature impacts the deviations on the mean

#comparison of the average rating for movies
movie_avgs <- edx_m %>%
  group_by(movieId) %>%
  summarize(b_i=mean(rating-mu)) 
#comparison of the average rating for the user
user_avgs <- edx_m %>%
  group_by(userId) %>%
  summarize(b_u=mean(rating-mu))
#comparison of the average rating for the user
aom_avgs <- edx_m %>%
  group_by(age_of_movie) %>%
  summarize(b_a=mean(rating-mu))
#comparison of the average rating for genres
genres_avgs <- edx_m %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu))
#comparison of the average rating month
month_avgs <- edx_m %>%
  group_by(date_month) %>%
  summarize(b_t=mean(rating-mu))
#comparison of the average rating total movie ratings
tr_avgs <- edx_m %>%
  group_by(total_ratings) %>%
  summarize(b_tr=mean(rating-mu))

p1 <- movie_avgs %>% ggplot(aes(b_i))+ geom_histogram(bins=30, color="black")
p2 <- user_avgs %>% ggplot(aes(b_u))+ geom_histogram(bins=30, color="black")
p4 <- aom_avgs %>% ggplot(aes(b_a))+ geom_histogram(bins=30, color="black")
p3 <- genres_avgs %>% ggplot(aes(b_g))+ geom_histogram(bins=30, color="black")
p5 <- month_avgs %>% ggplot(aes(b_t))+ geom_histogram(bins=30, color="black")
p6 <- tr_avgs %>% ggplot(aes(b_tr))+ geom_histogram(bins=30, color="black")

gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3, nrow=2)

# Model Evaluation

## Data Preprocessing
#function to process the data - derive the features to be used from the data set
data_process <- function(preData){
  processData <- preData %>% 
  mutate( movie_year = as.numeric(str_remove_all(str_sub(title, -6),"[^[:alnum:]]")),
          date_stamp = as_datetime(timestamp), year_t = year(date_stamp))
  processData <- processData %>% mutate(age_of_movie = as.factor(year_t - movie_year))
  processData <- processData %>% select(userId, movieId, rating, age_of_movie)
  return (as.data.frame(processData))
}


edx_m <- data_process(edx)
#split into test and train
test_index <- createDataPartition(y = edx_m$rating, times=1, p =0.1, list=FALSE)
train_set <- edx_m %>% slice(-test_index)
test_set <- edx_m %>% slice(test_index)

#remove users and movies not it the training set but in the test set
test_set <- test_set %>%
  semi_join(train_set, by="movieId") %>%
  semi_join(train_set, by="userId")

# RMSE function
RMSE <- function(TrueRatings, PredictedRatings){
  sqrt(mean((TrueRatings - PredictedRatings)^2))
}

## The Baseline Model

# Model based on the mean. the baseline
mu <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu)
rmse_results <- tibble(method="Naive_baseline", RMSE= naive_rmse)
rmse_results %>% knitr::kable()

## Regression Model

### Movie, User and AgeofMovie effects Model

#movie effects
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i=mean(rating-mu)) 
predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effects Model",  
                                     RMSE = model_1_rmse ))
#user effects
user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred=mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
#Age of movies
age_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(age_of_movie) %>% 
  summarize(b_a = mean(rating - mu - b_i - b_u))
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(age_avgs, by='age_of_movie') %>%
  mutate(pred=mu + b_i + b_u + b_a) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + AgeofMovie Effects Model",  
                                     RMSE = model_3_rmse ))

rmse_results %>% knitr::kable()

### Regularization

lambdas <- seq(4,6,0.1) # seq(0,10,0.5) was run prior to this with the larger step of 0.5
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating-mu)/(n()+l))
  
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  b_a <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(age_of_movie) %>%
    summarize(b_a = sum(rating - mu - b_i - b_u)/(n()+l))
  
  predict_ratings <- test_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_a, by="age_of_movie") %>%
    mutate(pred = mu + b_i + b_u + b_a) %>%
    pull(pred)
  
  return(RMSE(predict_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]
qplot(lambdas, rmses)
#optimal lambda
lambda

model_4_rmse <- rmses[lambda]
rmse_results <- bind_rows(
  rmse_results,data_frame(method="Movie + User + AgeofMovie Regularized Model",
                          RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()

#impact of regularization
movie_reg_m <- train_set %>%
  group_by(movieId) %>% summarize(b_i=sum(rating-mu)/(n()+lambda), n_i =n() )
movie_reg_u <- train_set %>%
  group_by(userId) %>% summarize(b_u=sum(rating-mu)/(n()+lambda), n_i =n() )

p1 <- tibble(movie_original = movie_avgs$b_i, movie_regularised = movie_reg_m$b_i,
             n = movie_reg_m$n_i) %>%
  ggplot(aes(movie_original, movie_regularised, size=sqrt(n))) + 
  geom_point(shape=1, alpha = 0.5, color=7)

p2 <- tibble(user_original = user_avgs$b_u, user_regularised = movie_reg_u$b_u,
             n = movie_reg_u$n_i) %>%
  ggplot(aes(user_original, user_regularised, size=sqrt(n))) + 
  geom_point(shape=1, alpha = 0.5, color=8)
gridExtra::grid.arrange(p1,p2, ncol=2)

## Matrix Factorization
### Recommenderlab

temp <- edx %>% select(userId, movieId, rating)
temp <- as.data.frame(temp)
rrm <- as(temp, "realRatingMatrix")
class(rrm)
movie_titles <- edx %>% distinct(movieId, title)
get_movie <- function(i){
  movie_titles %>% filter(movieId %in% i) %>% select(title)
}

#recommender function for 2000 users for learning
recom <- Recommender(rrm[1:2001], method="UBCF")
#predict for the user at 2020 (randomly)
recom_p <- predict(recom, rrm[2020], n=5)
#results
print("UBCF Results")
# get the titles from the movieid output
sapply(as(recom_p,"list") , function(i){
  movie_titles %>% filter(movieId %in% i) %>% select(title)
})
#recommender function
recom <- Recommender(rrm[1:2001], method="POPULAR")
#predict
recom_p <- predict(recom, rrm[2020], n=5)
#results
print("POPULAR Results")
# get the titles from the movieid output
sapply(as(recom_p,"list") , function(i){
  movie_titles %>% filter(movieId %in% i) %>% select(title)
})
#recommender function
recom <- Recommender(rrm[1:2001], method="SVD")
#predict
recom_p <- predict(recom, rrm[2020], n=5)
#results
print("SVD Results")
# get the titles from the movieid output
sapply(as(recom_p,"list") , function(i){
  movie_titles %>% filter(movieId %in% i) %>% select(title)
})

#taking the 90% quantile
rrm_m <- rrm[rowCounts(rrm)>quantile(rowCounts(rrm),0.9), 
              colCounts(rrm)>quantile(colCounts(rrm),0.9)]

#evaluation : good rating is 4 and above
#given=-5 implies that 5 ratings for the 20% users are excluded for testing.
e <- evaluationScheme(rrm_m, method="split", train=0.8, given=-5, goodRating=4)
#train the model
r1 <- Recommender(getData(e,"train"),"UBCF", 
                  param=list(normalize="center", method="pearson"))
#predict
p1 <- predict(r1, getData(e,"known"), type="ratings")

r2 <- Recommender(getData(e,"train"),"POPULAR")
p2 <- predict(r2, getData(e,"known"), type="ratings")

r3 <- Recommender(getData(e,"train"),"SVD")
p3 <- predict(r3, getData(e,"known"), type="ratings")

#results

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="RecommenderLab UBCF", 
                          RMSE = calcPredictionAccuracy(p1, getData(e,"unknown"))['RMSE'] ))
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="RecommenderLab POPULAR", 
                          RMSE = calcPredictionAccuracy(p2, getData(e,"unknown"))['RMSE'] ))
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="RecommenderLab SVD", 
                          RMSE = calcPredictionAccuracy(p3, getData(e,"unknown"))['RMSE'] ))
rmse_results %>% knitr::kable()

#cleanup
rm(e, edx_m, p1,p2,p3,r1,r2,r3, recom, recom_p, rrm, rrm_m, movie_titles)

### Recosystem

require(recosystem)
set.seed(1977, sample.kind = "Rounding")
#split the data for train and test
test_index <- createDataPartition(y = edx$rating, times=1, p =0.2, list=FALSE)
train_set <- edx %>% slice(-test_index)
test_set <- edx %>% slice(test_index)

#remove users and movies not it the training set but in the test set
test_set <- test_set %>%
  semi_join(train_set, by="movieId") %>%
  semi_join(train_set, by="userId")

#specify the train and the test data
train_data <- with(train_set, data_memory(user_index = userId,
                                          item_index = movieId,
                                          rating = rating))
test_data <- with(test_set, data_memory(user_index = userId,
                                        item_index = movieId,
                                        rating = rating))
#create the Model object
r <- recosystem::Reco()
#tuning the parameters; nthread is the number of threads to use and niter is the number of iterations.
opts <- r$tune(train_data, opts=list(dim = c(10,20,30),lrate = c(0.1,0.2),
                                     costp_l1=0, costq_l1=0,
                                     nthread= 4, niter=10))
#Train the model using the optimized parameters
r$train(train_data, opts = c(opts$min, nthread=4, niter=42))
#predicts the results
y_hat <- r$predict(test_data, out_memory())
#show the results
rmse_results <- bind_rows(
  rmse_results,data_frame(method="RecoSystem", 
                          RMSE = RMSE(test_set$rating, y_hat)))
## Collated RMSE results

rmse_results %>% knitr::kable()


# Final Validation
#preprocess the data - add the derived feature AgeofMovie
edx_f <- data_process(edx)
validation_f <- data_process(validation)

## Regularized Linear Model 

mu <- mean(edx$rating)
#lambda is the optimal lambda that was obtained
b_i <- edx_f %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating-mu)/(n()+lambda))

b_u <- edx_f %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))

b_a <- edx_f %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(age_of_movie) %>%
  summarize(b_a = sum(rating - mu - b_i - b_u)/(n()+lambda))

predict_ratings <- validation_f %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_a, by="age_of_movie") %>%
  mutate(pred = mu + b_i + b_u + b_a) %>%
  pull(pred)
Final_rmse <- RMSE(predict_ratings, validation_f$rating)

#final result
Final_rmse

rmse_results <- bind_rows(
  rmse_results,data_frame(method="Final: Movie + User + AgeofMovie Regularized Model (validation)",
                          RMSE = Final_rmse ))
rmse_results %>% knitr::kable()

validation %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  arrange(-pred) %>% group_by(title) %>% select(title) %>%
  head(5)

validation %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  arrange(pred) %>% group_by(title) %>% select(title) %>%
  head(5)

## Matrix Factorization

require(recosystem)
set.seed(1977, sample.kind = "Rounding")

#specify the train and the test data
train_data <- with(edx_f, data_memory(user_index = userId,
                                          item_index = movieId,
                                          rating = rating))
test_data <- with(validation_f, data_memory(user_index = userId,
                                        item_index = movieId,
                                        rating = rating))
#create the Model object
r <- recosystem::Reco()
#tuning the parameters; nthread is the number of threads to use and niter is the number of iterations.
opts <- r$tune(train_data, opts=list(dim = c(10,20,30),lrate = c(0.1,0.2),
                                     costp_l1=0, costq_l1=0,
                                     nthread= 4, niter=10))

#train the model
r$train(train_data, opts = c(opts$min, nthread=4, niter=42))
#predicts the results
y_hat <- r$predict(test_data, out_memory())

RMSE(validation_f$rating, y_hat)

rmse_results <- bind_rows(
  rmse_results,data_frame(method="RecoSystem (validation)", 
                          RMSE = RMSE(validation_f$rating, y_hat)))

rmse_results %>% knitr::kable()



#using recommenderlab on the movielens dataset
# the split will be done on the evaluationscheme step

temp <- movielens %>% select(userId, movieId, rating)
temp <- as.data.table(temp)
rrm <- as(temp, "realRatingMatrix")
#Using only most relevant users and movies due to low data/hardware ratio
rrm_m <- rrm[rowCounts(rrm)>quantile(rowCounts(rrm),0.9), 
             colCounts(rrm)>quantile(colCounts(rrm),0.9)]
#evaluation 
e <- evaluationScheme(rrm_m, method="split", train= 0.9, given=-5, goodRating=4)

r1 <- Recommender(getData(e,"train"),"POPULAR")
p1 <- predict(r1, getData(e,"known"), type="ratings")

r2 <- Recommender(getData(e,"train"),"UBCF", 
                  param=list(normalize="center", method="pearson"))
p2 <- predict(r2, getData(e,"known"), type="ratings")

r3 <- Recommender(getData(e,"train"),"SVD")
p3 <- predict(r3, getData(e,"known"), type="ratings")

rmse_results <- bind_rows(
  rmse_results,data_frame(method="Movielens RecommenderLab POPULAR (movielens)", 
                          RMSE = calcPredictionAccuracy(p1, getData(e,"unknown"))['RMSE'] ))
rmse_results <- bind_rows(
  rmse_results,data_frame(method="Movielens RecommenderLab UBCF (movielens)", 
                          RMSE = calcPredictionAccuracy(p2, getData(e,"unknown"))['RMSE'] ))
rmse_results <- bind_rows( 
  rmse_results,data_frame(method="Movielens RecommenderLab SVD (movielens)", 
                          RMSE = calcPredictionAccuracy(p3, getData(e,"unknown"))['RMSE'] ))
##FINAL RESULTS
rmse_results %>% knitr::kable()
##FINAL RESULTS chart
rmse_results %>% 
  ggplot(aes(reorder(method,RMSE),RMSE)) + geom_point() +
  geom_bar(stat="identity", fill= 4) +
  coord_flip() +
  theme(axis.text.y = element_text(size=8)) +
  ylab("RMSE") + xlab('')

######################veneet.bhardwaj@gmail.com#################################