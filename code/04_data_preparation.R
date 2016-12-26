
#---------------------------------------------------------------------------------------
# 4 - REDUCING THE MATRIX
#---------------------------------------------------------------------------------------

getReducedMatrix <- function() {

# create shorter matrix 421 x 444
real_ratings_matrix_small <- real_ratings_matrix_type[rowCounts(real_ratings_matrix_type)>50,
                                                      colCounts(real_ratings_matrix_type)>50]

min_movies <- quantile(rowCounts(real_ratings_matrix_small), 0.98)
min_users <- quantile(colCounts(real_ratings_matrix_small), 0.98)

g1 <- image(real_ratings_matrix_small[rowCounts(real_ratings_matrix_small) > min_movies,
                                colCounts(real_ratings_matrix_small) > min_users], main = "Reduced matrix - Heatmap of the top users and movies",
            xlab = 'Movies', 
            ylab = 'Users')

real_avg_ratings_per_user <- rowMeans(real_ratings_matrix_small)

g2 <- qplot(real_avg_ratings_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Reduced matrix - distribution of the average rating per user") + 
  xlab('Average rating') + 
  ylab('Count')

print(g1)
print(g2)

}

getReducedMatrix()

