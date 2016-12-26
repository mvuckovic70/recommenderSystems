
#---------------------------------------------------------------------------------------
# 3 - MATRIX VISUALIZATION
#---------------------------------------------------------------------------------------

cat("\014") 
# source(paste0(pathsCodes,'00_init.R'))
# source(paste0(pathsCodes,'01_cleaning_transforming.R'))
# source(paste0(pathsCodes,'02_data_exploration.R'))
set.seed(42)

# heatmaps for warmup

getMatrixHeatmap <- function(){

g1 <- image(real_ratings_matrix_type, 
            main = "Heatmap of the full rating matrix", xlab='Movies', ylab='Users')

g2 <- image(real_ratings_matrix_type[1:25, 1:25], main = "Heatmap of the first 25 rows and
      columns", xlab='Movies', ylab='Users')

# minimum movies per user, and user per movie

min_movies_per_user <- quantile(rowCounts(real_ratings_matrix_type), 0.95)
min_users_per_movie <- quantile(colCounts(real_ratings_matrix_type), 0.99)

g3 <- image(real_ratings_matrix_type[rowCounts(real_ratings_matrix_type) > min_movies_per_user,
                               colCounts(real_ratings_matrix_type) > min_users_per_movie], 
      main = "Heatmap of the top users and movies",
      xlab='Movies', ylab='Users')

print(g1)
print(g2)
print(g3)

cat("\014") 

}

getMatrixHeatmap()
