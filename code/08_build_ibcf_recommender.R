
#---------------------------------------------------------------------------------------
# 8 - BUILD AND TRAIN AN IBCF RECOMMENDER
#---------------------------------------------------------------------------------------

getIBCFrecommender <- function(){

# build a recommender model with a square matrix 444 x 444
model <- Recommender(data = train, method = "IBCF", parameter = list(k = 30))
model_details <- getModel(model)
dim(model_details$sim)

# show heatmap of movie distribution per user on matrix head
g1 <- image(model_details$sim[1:15, 1:15], main = "IBCF recommender - Heatmap of the first rows and columns")

# 444 users rated 30 movies, each user contain only 30 elements (ratings)
model_row_sums <- rowSums(model_details$sim > 0)
table(model_row_sums)

#  
model_col_sums <- colSums(model_details$sim > 0)
table(model_col_sums)

# check for movie similarity

g2 <- qplot(model_col_sums) + stat_bin(binwidth = 1) + 
  ggtitle("IBCF recommender - Distribution of movies per ratings") + 
  xlab('Number of movies') +
  ylab('Number of ratings')

# find most similar movies
most_similar_movies_count <- order(model_col_sums, decreasing = TRUE)[1:6]
most_similar_movies_id <- rownames(model_details$sim)[most_similar_movies_count]
most_similar_movies <- cbind(most_similar_movies_id, most_similar_movies_count)
colnames(most_similar_movies)[1:2] <- c('movieId' , 'count')
most_similar_movies <- merge(most_similar_movies, movies, by='movieId')
most_similar_movies$genres <- NULL
most_similar_movies$count <- as.integer(as.character(most_similar_movies$count))

# similarity plot
g3 <- ggplot(most_similar_movies, aes(fill=movieId, x=title, y=count)) +   
  geom_bar(aes(fill = movieId), position = "dodge", stat="identity") + 
  ggtitle("IBCF recommender - Movies with most elements") + 
  xlab('Movie') + 
  ylab('Count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(g1)
print(g2)
print(g3)

return(model)

}

model <- getIBCFrecommender()

cat("\014")
