
#---------------------------------------------------------------------------------------
# 11 - EVALUATION OF THE RATINGS
#---------------------------------------------------------------------------------------

# creating testing environment
kfold_eval <- evaluationScheme(data = real_ratings_matrix_type, 
                               method = "cross-validation",
                               k = 4, given = 15, goodRating = 3)

kfold_sets <- sapply(kfold_eval@runsTrain, length)

# build recommender
eval_recommender <- Recommender(data = getData(kfold_eval, "train"),
                                method = 'IBCF', 
                                parameter = NULL)

# make a prediction
eval_prediction <- predict(object = eval_recommender, 
                           newdata = getData(kfold_eval,'known'),
                           n=10,
                           type='ratings')

# histogram
qplot(rowCounts(eval_prediction)) + geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user")

# compute the accuracy
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(kfold_eval, "unknown"), 
                                        byUser = TRUE)

head(eval_accuracy)

# RMSE by user

qplot(eval_accuracy[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")

# model accuracy
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(kfold_eval, "unknown"), 
                                        byUser = FALSE) 
