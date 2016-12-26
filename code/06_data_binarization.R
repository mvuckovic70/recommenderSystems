
#---------------------------------------------------------------------------------------
# 6 - DATA BINARIZATION
#---------------------------------------------------------------------------------------

getBinaryMatrix <- function(){

real_ratings_matrix_small <- real_ratings_matrix_type[rowCounts(real_ratings_matrix_type)>50,
                                                      colCounts(real_ratings_matrix_type)>50]

# create binarized matrix consisting of 0s and 1s
ratings_movies_watched_bin <- binarize(real_ratings_matrix_small, minRating = 1)

# define sample
min_movies_bin <- quantile(rowCounts(real_ratings_matrix_small), 0.95)
min_users_bin <- quantile(colCounts(real_ratings_matrix_small), 0.95)

image(ratings_movies_watched_bin[rowCounts(real_ratings_matrix_small) > min_movies_bin,
                                 colCounts(real_ratings_matrix_small) > min_users_bin], 
      main = "Binary matrix - Heatmap of the top users and movies", x='Movies', y='Users')

# split ratings to good and bad, threshold = 3

ratings_movies_bin_good <- binarize(real_ratings_matrix_small, minRating = 3)

image(ratings_movies_bin_good[rowCounts(real_ratings_matrix_small) > min_movies_bin, 
                              colCounts(real_ratings_matrix_small) > min_users_bin], 
      main = "Binary matrix - Heatmap of the top users and movies", xlab = 'Movies', ylab='Users')
}

getBinaryMatrix()
