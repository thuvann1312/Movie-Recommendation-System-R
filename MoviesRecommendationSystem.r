# install.packages("recommenderlab")
# install.packages("data.table")
# install.packages("reshape2")
# install.packages("ggplot2")

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

# Retrieving data from movies.csv into movie_data dataframe and ratings.csv into rating_data.
movies <- read.csv("D:/R/IMDB-Dataset/movies.csv", encoding="UTF-8")
View(movies)
ratings <- read.csv("D:/R/IMDB-Dataset/ratings.csv", encoding="UTF-8")
View(ratings)

str(movies)
summary(movies)
head(movies)
summary(ratings)
head(ratings)

# Data Pre-processing
# The userId column, as well as the movieId column, consist of integers 
# -> Convert the genres present in the movie_data dataframe into a more usable format by the users. 
# First create matrix that comprises of corresponding genres for each of the films.
movie_genre <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
library(data.table)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]',
                                        type.convert=TRUE),
                              stringsAsFactors=FALSE) 

colnames(movie_genre2) <- c(1:10)
list_genre <- c("Action", "Adventure", "Animation", "Children",
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
}
str(genre_mat2)


# Create a ‘search matrix’ by specifying the genre present in our list.
SearchMatrix <- cbind(movies[,1:2], genre_mat2[])
head(SearchMatrix)    

ratingMatrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
# Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

# Parameters that provide us various options for building recommendation systems
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
recommendation_model$IBCF_realRatingMatrix$parameters

# Exploring Similar Data
# Suggesting movies to the users that are based on collecting preferences from many other users.
similarity_mat <- similarity(ratingMatrix[1:4, ], method = "cosine", which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

movie_similarity <- similarity(ratingMatrix[, 1:4], method = "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")

# Extract the most unique ratings
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

Table_of_Ratings <- table(rating_values) # Creating a count of movie ratings
Table_of_Ratings

# Most Viewed Movies Visualization
movie_views <- colCounts(ratingMatrix) # Count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # Create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # Sort by number of views
table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.character(subset(movies,
                                              movies$movieId == table_views[index,1])$title)
}
table_views[1:6,]

# Bar plot for the total number of views of the top films.
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = "#BE658D") +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Total Views of the Top Films")

# Heatmap of Movie Ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

# Performing Data Preparation
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings

# Delineate matrix of relevant users 
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

# Visualize the distribution of the average ratings per user.
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill= "DE7D63", col= I("White")) +
  ggtitle("Distribution of the average rating per user")

# Data Normalization
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")

# Performing Data Binarization
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

# Movies_watched <- binarize(movie_ratings, minRating = 1)
good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

# Collaborative Filtering System
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

# Building the Recommendation System using R
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommend_model <- Recommender(data = training_data,
                               method = "IBCF",
                               parameter = list(k = 30))
recommend_model
class(recommend_model)

# 20 Items
model_info <- getModel(recommend_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

# The sum of rows and columns with the similarity of the objects above 0.
# Visualize the sum of columns through a distribution
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill= I("Purple"), col=I("Yellow"))+ ggtitle("Distribution of the column count")

# Create a top_recommendations variable which will be initialized to 10, specifying the number of films to each user.
top_recommendations <- 10 # The number of items to recommend to each user
predicted_recommendations <- predict(object = recommend_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]] # Recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movies,
                                             movies$movieId == movies_user1[index])$title)
}
movies_user2

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # Matrix with the recommendations for each user
# Dim(recc_matrix)
recommendation_matrix[,1:4]

# Bar Chart of Distribution of the Number of Items for IBCF
number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Distribution of the Number of Items for IBCF"
qplot(number_of_items, fill=I("Lightblue"), col=I("Gray")) + ggtitle(chart_title)

# Perform Number of items
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
for(i in 1:4) {
  table_top[i,1] <- as.character(subset(movies,
                                        movies$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie Title", "No. of Items")
head(table_top)

percent_train = 0.8
#min(rowCounts(ratings.n))
items_to_keep = 5        # items to use for each user
rating_threshold = 3      # good rating implies >=3
n_eval = 1                # number of times to run eval

eval_sets = evaluationScheme(data = movie_ratings, method = "split",
                             train = percent_train, given = items_to_keep,
                             goodRating = rating_threshold, k = n_eval)


##-----EVALUATIONS----
#UBCF system
eval_recommender = Recommender(data = getData(eval_sets, "train"),
                               method = "UBCF", parameter = NULL)
items_to_recommend = 10
eval_prediction = predict(object = eval_recommender,
                          newdata = getData(eval_sets, "known"),
                          n = items_to_recommend,
                          type = "ratings")
eval_accuracy = calcPredictionAccuracy(x = eval_prediction,
                                       data = getData(eval_sets, "unknown"),
                                       byUser = TRUE)
head(eval_accuracy)

#IBCF

eval_recommender1 = Recommender(data = getData(eval_sets, "train"),
                                method = "IBCF", parameter = list(k=30))
eval_prediction1 = predict(object = eval_recommender1,
                           newdata = getData(eval_sets, "known"),
                           n = items_to_recommend,
                           type = "ratings")
eval_accuracy1 = calcPredictionAccuracy(x = eval_prediction1,
                                        data = getData(eval_sets, "unknown"),
                                        byUser = TRUE)
head(eval_accuracy1)

#Evaluation using different similarity
models_to_evaluate = list(IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
                          IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
                          UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
                          UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
                          random = list(name = "RANDOM", param=NULL))

n_recommendations = c(1, 3, 5, 10, 15, 20)
results = evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)

#Compare
plot(results, y = "ROC", annotate = 1, legend="topleft")
title("ROC Curve")

plot(results, y = "prec/rec", annotate=1)
title("Precision-Recall")

#Evaluation comparing algorithms
algorithm = list( "random items" = list(name="RANDOM", param=NULL),
                  "popular items" = list(name="POPULAR", param=NULL),
                  "user-based CF" = list(name="UBCF", param=list(nn=50)),
                  "item-based CF" = list(name="IBCF", param=list(k=50)),
                  "SVD approximation" = list(name="SVD", param=list(k = 50)))

results1 = evaluate(x = eval_sets, method = algorithm, n = n_recommendations)

plot(results1, y = "prec/rec", annotate=1,)
title("Precision-Recall")