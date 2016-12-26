#---------------------------------------------------------------------------------------
# 0 - INIT
#---------------------------------------------------------------------------------------

#-------------------------
# 0.1 Clear variable space
#-------------------------

rm(list=ls())

#-------------------------------
# 0.2 Function to load libraries
#-------------------------------

getLibraries <- function(){
x<-c('sqldf', 'data.table', 'recommenderlab', 'ggplot2')
lapply(x, require, character.only = TRUE)
cat("\014") 
}

getLibraries()

#------------------
# 0.3 Load datasets
#------------------

paths <- list(data = 'D:/Projects/datasets/imdb/ml-latest-small/')
pathsCodes <- 'D:/Projects/R/algorithms/recommender systems/movieLenseWeb/codes/'

links <- read.csv(paste0(paths$data, 'links.csv'))
movies <- read.csv(paste0(paths$data, 'movies.csv'))
ratings <- read.csv(paste0(paths$data, 'ratings.csv'))
tags <- read.csv(paste0(paths$data, 'tags.csv'))

cat("\014", 'Done.')

