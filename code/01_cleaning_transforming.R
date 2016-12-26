
#---------------------------------------------------------------------------------------
# 1 - DATA CLEANING AND TRANSFORMATIONS
#---------------------------------------------------------------------------------------

cat("\014") 
# source(paste0(pathsCodes,'00_init.R'))

#-------------------
# 1.1 Extract genres
#-------------------

getGenres <- function()
{
  
  # extract unique values of genres and rename column  
  genres <- as.data.frame(unique(movies$genres))
  colnames(genres)[1] <- 'genres'
  
  # columns in genres table consists of multiple genres, so splitting now and renaming
  genres2 <- as.data.frame(tstrsplit(genres[,c('genres')], '[|]', 
                                     type.convert=TRUE), 
                           stringsAsFactors=FALSE)
  
  colnames(genres2)<- c(1:ncol(genres2))
  
  # create one-column values from multi-column values of the same type
  genres3 <- stack(genres2)
  genres3$ind <- NULL
  
  # create unique values from genres3
  genres4 <- na.omit(as.data.frame(unique(genres3$values)))
  colnames(genres4)[1] <- 'genres'
  cat("\014") 
  return(genres4)
}

genres <- getGenres()

#-------------------------------
# 1.2 Extract users from ratings
#-------------------------------

getUsers <- function(){
  # unique users
  users <- as.data.frame(unique(ratings$userId))
  colnames(users)[1] <- 'userid'
  cat("\014")
  return(users)
}

users <- getUsers()

# assign movieid to a rowname
row.names(movies) <- movies$movieId

#----------------------------------------
# 1.3 Create a wide data frame of ratings
#----------------------------------------

getRatings <- function()
{
  ratings$timestamp <- NULL
  ratings_matrix <- reshape(data=ratings, 
                            direction='wide', 
                            idvar='userId', 
                            timevar='movieId', 
                            v.names='rating')
  colnames(ratings_matrix) <- gsub('rating.', '', colnames(ratings_matrix))
  rownames(ratings_matrix) <- ratings_matrix$userId
  ratings_matrix$userId <- NULL
  cat("\014")
  return(ratings_matrix)
}

ratings_matrix <- getRatings()

#-----------------------------------
# 1.4 Create a ratings matrix R type
#-----------------------------------

ratings_matrix_type <- as(ratings_matrix, 'matrix')

#-----------------------------------------
# 1.5 Convert to a recommender type matrix
#-----------------------------------------

real_ratings_matrix_type <- as(ratings_matrix_type, 'realRatingMatrix')

cat("\014", 'Done.')