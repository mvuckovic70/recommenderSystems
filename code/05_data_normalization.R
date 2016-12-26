
#---------------------------------------------------------------------------------------
# 5 - DATA NORMALIZATION
#---------------------------------------------------------------------------------------

getNormalizedMatrix <- function(){

real_ratings_matrix_small <- real_ratings_matrix_type[rowCounts(real_ratings_matrix_type)>50,
                                                        colCounts(real_ratings_matrix_type)>50]

min_movies <- quantile(rowCounts(real_ratings_matrix_small), 0.98)
min_users <- quantile(colCounts(real_ratings_matrix_small), 0.98)

normalized_ratings_matrix <- normalize(real_ratings_matrix_small)
sum(rowMeans(normalized_ratings_matrix) > 0.00001)

image(normalized_ratings_matrix[rowCounts(normalized_ratings_matrix) > min_movies,
                                colCounts(normalized_ratings_matrix) > min_users], 
      main = "Normalized matrix - Heatmap of the top users and movies")
}

getNormalizedMatrix()
