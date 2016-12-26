
#---------------------------------------------------------------------------------------
# 2 - DATA EXPLORATION
#---------------------------------------------------------------------------------------

cat("\014") 
# source(paste0(pathsCodes,'00_init.R'))
# source(paste0(pathsCodes,'01_cleaning_transforming.R'))
set.seed(42)

#-------------------------------------------
# 2.1. Similarity matrix for users and items
#-------------------------------------------

getSimilarity <- function(){

similarity_users <- similarity(real_ratings_matrix_type[1:6, ], 
                               method = "cosine", 
                               which = "users")

similarity_movies <- similarity(real_ratings_matrix_type[,1:6], 
                               method = "cosine", 
                               which = "items")

# plot similarity matrices samples

# as.matrix(similarity_users)
# as.matrix(similarity_items)

image(as.matrix(similarity_users), main = "Similarity of first 6 users")
image(as.matrix(similarity_movies), main = "Similarity of first 6 movies")
cat("\014") 
}

getSimilarity()

#-----------------------
# 2.2. Ratings frequency
#-----------------------

getRatingsFrequency <- function(){

# explore ratings frequency (.5 values problem)
ratings_frequency <- as.vector(real_ratings_matrix_type@data)
unique(ratings_frequency)
ratings_frequency <- factor(ratings_frequency)

g1 <- qplot(ratings_frequency) + 
  ggtitle("Distribution of the ratings with .5 decimal step and zeros") +
  xlab('Movie rating') + 
  ylab('Rating frequency') 

# excluding missing ratings and plotting rating frequency

ratings_frequency <- subset(ratings_frequency, ratings_frequency!=0)
g2 <- qplot(ratings_frequency) + 
  ggtitle("Distribution of the ratings with .5 decimal step") +
  xlab('Movie rating') + 
  ylab('Rating frequency') 

# creating ratings frequency table by rounding .5 figures to the higher round number

ratings_frequency_table <- data.frame(table(ratings_frequency))
colnames(ratings_frequency_table)[1:2] <- c('rating', 'frequency')
ratings_frequency_table <- subset(ratings_frequency_table, rating!=0)
ratings_frequency_table$rating <- as.numeric(as.character(ratings_frequency_table$rating))
ratings_frequency_table$rating <- ratings_frequency_table$rating + 0.01 
ratings_frequency_table$rating <- round(ratings_frequency_table$rating)
ratings_frequency_table <- aggregate(ratings_frequency_table$frequency, 
                                     by=list(ratings_frequency_table$rating), 
                                     FUN=sum)

colnames(ratings_frequency_table)[1:2] <- c('rating', 'frequency')
ratings_frequency_table$rating <- factor(ratings_frequency_table$rating)

# plotting rating frequencies with rounded ratings

g3 <- ggplot(ratings_frequency_table, aes(fill=rating, x=rating, y=frequency)) +   
  geom_bar(aes(fill = rating), position = "dodge", stat="identity") + 
  ggtitle("Distribution of movie ratings") + 
  xlab('Movie rating') + 
  ylab('Rating frequency') 

print(g1)
print(g2)
print(g3)

cat("\014")
}

getRatingsFrequency()


#------------------------------
# 2.3. Top movies ratings count
#------------------------------

getRatingsTopMoviesCount <- function(){

ratings_per_movie <- colCounts(real_ratings_matrix_type)
ratings_per_movie <- data.frame(movie = names(ratings_per_movie), ratings = ratings_per_movie)
colnames(ratings_per_movie)[1] <- 'movieId'
ratings_per_movie <- merge(ratings_per_movie, movies, by='movieId', all.x=TRUE)
ratings_per_movie <- ratings_per_movie[order(ratings_per_movie$ratings, decreasing =TRUE), ]
ratings_per_movie$genres <- NULL

g1 <- ggplot(ratings_per_movie[1:6, ], aes(fill=ratings, x = title, y = ratings, label=ratings)) +
  geom_bar(aes(fill = ratings), stat="identity") + 
  geom_text(position = position_dodge(0.9), vjust = -0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Most rated movies") +
  xlab('Movie title') + 
  ylab('Rating count') 

print(g1)

cat("\014")
}

getRatingsTopMoviesCount()

#--------------------------------
# 2.4. Top movies average ratings
#--------------------------------

getRatingsTopMoviesAvg <- function() {

ratings_per_movie <- colCounts(real_ratings_matrix_type)
ratings_per_movie <- data.frame(movie = names(ratings_per_movie), ratings = ratings_per_movie)
colnames(ratings_per_movie)[1] <- 'movieId'
ratings_per_movie <- merge(ratings_per_movie, movies, by='movieId', all.x=TRUE)
ratings_per_movie <- ratings_per_movie[order(ratings_per_movie$ratings, decreasing =TRUE), ]
ratings_per_movie$genres <- NULL
  
top_rated_movies <- colMeans(real_ratings_matrix_type)
top_rated_movies <- data.frame(movieId = names(top_rated_movies), ratings = top_rated_movies)
top_rated_movies <- merge(top_rated_movies, ratings_per_movie, by='movieId', all.x=TRUE)
colnames(top_rated_movies)[2:3] <- c('score','ratings')
top_rated_movies <- top_rated_movies[order(top_rated_movies$ratings, decreasing =TRUE),]
top_rated_movies_20 <- top_rated_movies[1:20, ]
top_rated_movies_20 <- top_rated_movies_20[order(top_rated_movies_20$score, decreasing = TRUE),]

g1 <- ggplot(top_rated_movies_20, aes(fill=score, x = title, y = score, labels=score)) +
  geom_bar(aes(fill = score), stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label=round(score,2)), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Top rated movies") +
  xlab('Movie title') + 
  ylab('Rating average score') 
  
print(g1)

cat("\014")
}

getRatingsTopMoviesAvg()

#-----------------------
# 2.5. Most active users
#-----------------------
 
getMostActiveUsers <- function(){

ratings_per_user <- rowCounts(real_ratings_matrix_type)
ratings_per_user <- data.frame(user = names(ratings_per_user), ratings = ratings_per_user)
colnames(ratings_per_user)[1] <- 'userId'
ratings_per_user <- ratings_per_user[order(ratings_per_user$ratings, decreasing =TRUE), ]

g1 <- ggplot(ratings_per_user[1:10, ], aes(fill=ratings, x = userId, y = ratings, label=ratings)) +
  geom_bar(aes(fill = ratings), stat="identity") + 
  geom_text(position = position_dodge(0.9), vjust = -0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Most active users") +
  xlab('User ID') + 
  ylab('Number of ratings') 


print(g1)

cat("\014")
}

getMostActiveUsers()

#-------------------------
# 2.6. Average user rating
#-------------------------

getUserRatingAvg <- function() {

average_ratings_per_user <- rowMeans(real_ratings_matrix_type)

g1 <- qplot(average_ratings_per_user, geom='histogram', binwidth = 0.1) +
  ggtitle("Average rating frequency") + 
  xlab('Count') + ylab('Average rating') 
 

print(g1)

# cat("\014")
}

getUserRatingAvg()
