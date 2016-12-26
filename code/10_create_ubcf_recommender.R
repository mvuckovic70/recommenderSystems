
#---------------------------------------------------------------------------------------
# 10 - CREATE A UBCF RECOMMENDER
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# 10.1 - CREATE UBCF RECOMMENDATIONS MATRIX
#---------------------------------------------------------------------------------------

testRecommenderUBCF <- function(){

# build a recommender model with a square matrix 444 x 444
model_ubcf <- Recommender(data = train, method = "UBCF")
model_ubcf_details <- getModel(model)
dim(model_ubcf_details$sim)

# predict
recommendation_prediction_ubcf <- predict(object = model_ubcf, newdata = test, n = 5)

# create a matrix of predictions for 85 users in a test set

recommendation_matrix_ubcf <- sapply(recommendation_prediction_ubcf@items, 
                                     function(x){colnames(real_ratings_matrix_type)[x]})

most_recommended_movies_ubcf <- factor(table(recommendation_matrix_ubcf))

recommendation_table_ubcf <- as.data.frame(most_recommended_movies_ubcf)
colnames(recommendation_table_ubcf)[1] <- 'count'
recommendation_table_ubcf$movieId <- row.names(recommendation_table_ubcf)
recommendation_table_ubcf <- merge(recommendation_table_ubcf, movies, by='movieId')
recommendation_table_ubcf$genres <- NULL

# find top 15 most recommended movies
recommendation_table_ubcf[order(recommendation_table_ubcf$count, decreasing =TRUE),]
recommendation_table_ubcf_top15 <- head(recommendation_table_ubcf
                                        [order(recommendation_table_ubcf$count, 
                                               decreasing =TRUE),],15)

# plot top 15 most recommendations
chart_title_ubcf <- "Top 15 most recommended movies UBCF"
#qplot(recommendation_table_ubcf) + ggtitle(chart_title)

ggplot(recommendation_table_ubcf_top15, aes(fill=count, x=title, y=count)) +   
  geom_bar(aes(fill = count), position = "dodge", stat="identity") + 
  ggtitle(chart_title_ubcf) + 
  xlab('Movie') + 
  ylab('Number of recommendations') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

return(recommendation_matrix_ubcf)

print(g6)

cat("\014")
}

recommendation_matrix_ubcf <- testRecommenderUBCF()

#---------------------------------------------------------------------------------------
# 10.2 - FIND A SINGLE USER RECOMMENDATION
#---------------------------------------------------------------------------------------

getSingleUBCF <- function(x){

# function to display recommended movies for chosen user
  
# predict
recommendation_prediction_ubcf <- predict(object = model_ubcf, newdata = test, n = 5)

  prediction_user_ubcf <- data.frame(recommendation_prediction_ubcf@items[[x]])
  colnames(prediction_user_ubcf) <- 'movieId'
  prediction_user_ubcf <- merge(prediction_user_ubcf, movies, by = 'movieId')
  prediction_user_ubcf$genres <- NULL
  cat("\014") 
  paste0(prediction_user_ubcf$title)
}

# synthax: getSingleUBCF -> example for userid=1:
getSingleUBCF(1)
