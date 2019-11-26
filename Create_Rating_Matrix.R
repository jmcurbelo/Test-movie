#' Esta función duvuelve la Matriz (Matrix Raitings) como un objeto de 
#' tipo realRatingsMatrix 

Create_Ratings_Matrix <- function(){
        source("Read Files of Movies.R")
        library(recommenderlab)
        ratings_matrix <- as(ReadFiles()$Ratings[,1:3],"realRatingMatrix")
        return(ratings_matrix)
}


