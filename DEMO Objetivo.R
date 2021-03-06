library(recommenderlab)

source("Read Files of Movies.R")

movies <- ReadFiles()$Movies
moviesId <- movies$movieId

Seleccionar_Peliculas <- function(ids){
        
        library(dplyr)
        muestra <- movies %>%
                filter( (movieId == ids[1]) | (movieId == ids[2]) |
                                (movieId == ids[3]) | (moviesId == ids[4]) |
                                moviesId == ids[5])
        return(muestra)
        
}




####################################################



Recomendar_Pelicula <- function(recomendador, ids){
        
        prueba <- Seleccionar_Peliculas(ids)
        
        matriz <- matrix(NA, nrow = 1, ncol = 9724)
        
        matriz[1,which(moviesId==prueba$movieId[1])]<- 5
        matriz[1,which(moviesId==prueba$movieId[2])]<- 5
        matriz[1,which(moviesId==prueba$movieId[3])]<- 4.9999
        matriz[1,which(moviesId==prueba$movieId[4])]<- 5
        matriz[1,which(moviesId==prueba$movieId[5])]<- 5
        
        rating_prueba <- as(matriz, "realRatingMatrix")
        
        pred <- predict(recomendador, rating_prueba, n=5)
        
        p_ratings <- predict(recomendador, rating_prueba, type = "ratings")
        p_matrix_rating <- as(p_ratings, "matrix")
        
        ###################################################
        top_ratings <- p_matrix_rating[1,]
        
        top_ratings <- sort(top_ratings, decreasing = TRUE)
        
        
        ###################################################
        
        id_movies <- as.integer(as(pred, "list")[[1]])
        
        ratings_list <- numeric()
        ratings_list[1] <-  p_matrix_rating[1,which(id_movies[1]==moviesId)]
        ratings_list[2] <-  p_matrix_rating[1,which(id_movies[2]==moviesId)]
        ratings_list[3] <-  p_matrix_rating[1,which(id_movies[3]==moviesId)]
        ratings_list[4] <-  p_matrix_rating[1,which(id_movies[4]==moviesId)]
        ratings_list[5] <-  p_matrix_rating[1,which(id_movies[5]==moviesId)]
        
        library(dplyr)
        top5_movies <- movies %>%
                select(movieId, title, genres) %>%
                filter( (movieId == id_movies[1]) | (movieId == id_movies[2]) |
                                (movieId == id_movies[3]) | (movieId == id_movies[4]) |
                                (movieId == id_movies[5]))
                 
        
        top5_movies_ordenados <- top5_movies[order(match(top5_movies[,1], id_movies)),]
        
        top5_movies_ordenados$ratings <- ratings_list
        
        print("En base a la elección y los Ratings de estas películas")
        print("")
        print(prueba)
        print("")
        print("Las recomendaciones para ud según sus gustos son:")
        print(top5_movies_ordenados)
        
        
        ###########################################3
        # top ratings
        print("Top ratings")
        print("")
        print(top_ratings[1:7])
        
        ################################################

}


