
#---------------------------------------------------------------------------------------
# 9 - CREATE AN IBCF RECOMMENDER
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# 9.1 - CREATE IBCF RECOMMENDATIONS MATRIX
#---------------------------------------------------------------------------------------

testRecommenderIBCF <- function(){

# make a prediction for 85 users, 5 movies recommended for each user
recommendation_prediction <- predict(object = model, newdata = test, n = 5)
class(recommendation_prediction)
slotNames(recommendation_prediction)


# create a matrix of predictions for 85 users in a test set
recommendation_matrix <- sapply(recommendation_prediction@items, 
                                function(x){colnames(real_ratings_matrix_type)[x]})

most_recommended_movies <- factor(table(recommendation_matrix))

recommendation_table <- as.data.frame(most_recommended_movies)
colnames(recommendation_table)[1] <- 'count'
recommendation_table$movieId <- row.names(recommendation_table)
recommendation_table <- merge(recommendation_table, movies, by='movieId')
recommendation_table$genres <- NULL

# find top 15 most recommended movies
recommendation_table[order(recommendation_table$count, decreasing =TRUE),]
recommendation_table_top15 <- head(recommendation_table
                                   [order(recommendation_table$count, 
                                          decreasing =TRUE),],15)

# plot top 15 most recommendations
chart_title <- "Top 15 most recommended movies IBCF"

g1 <- ggplot(recommendation_table_top15, aes(fill=count, x=title, y=count)) +   
  geom_bar(aes(fill = count), position = "dodge", stat="identity") + 
  ggtitle(chart_title) + 
  xlab('Movie') + 
  ylab('Number of recommendations') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

return(recommendation_matrix)

print(g1)

cat("\014")

}

recommendation_matrix_ibcf <- testRecommenderIBCF()

#---------------------------------------------------------------------------------------
# 9.2 - FIND A SINGLE USER RECOMMENDATION
#---------------------------------------------------------------------------------------

getSingleIBCF <- function(x){
  
  # function to display recommended movies for chosen user
  
  # make a prediction for 85 users, 5 movies recommended for each user
  recommendation_prediction <- predict(object = model, newdata = test, n = 5)
  class(recommendation_prediction)
  slotNames(recommendation_prediction)
  
    prediction_user <- data.frame(recommendation_prediction@items[[x]])
    colnames(prediction_user) <- 'movieId'
    prediction_user <- merge(prediction_user, movies, by = 'movieId')
    prediction_user$genres <- NULL
    cat("\014") 
    paste0(prediction_user$title)
  }

# synthax: getSingleIBCF -> example for userid=1:
getSingleIBCF(1)

  