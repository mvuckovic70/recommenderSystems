
#---------------------------------------------------------------------------------------
# 7 - SPLIT TRAIN / TEST
#---------------------------------------------------------------------------------------

splitTrain <- function(){

  real_ratings_matrix_small <- real_ratings_matrix_type[rowCounts(real_ratings_matrix_type)>50,
                                                        colCounts(real_ratings_matrix_type)>50]
  
samplesize <- floor(0.8 * nrow(real_ratings_matrix_small))
set.seed(1)
train_rule <- sample(seq_len(nrow(real_ratings_matrix_small)), size = samplesize)

train <- real_ratings_matrix_small[train_rule, ]
test <- real_ratings_matrix_small[-train_rule, ]
return(train)
}



splitTest <- function(){
  
  real_ratings_matrix_small <- real_ratings_matrix_type[rowCounts(real_ratings_matrix_type)>50,
                                                        colCounts(real_ratings_matrix_type)>50]
  
  samplesize <- floor(0.8 * nrow(real_ratings_matrix_small))
  set.seed(1)
  train_rule <- sample(seq_len(nrow(real_ratings_matrix_small)), size = samplesize)
  
  train <- real_ratings_matrix_small[train_rule, ]
  test <- real_ratings_matrix_small[-train_rule, ]
  return(test)
}

train <- splitTrain()
test <- splitTest()
cat("\014") 
splitTrain()
splitTest()


train <- splitTrain()