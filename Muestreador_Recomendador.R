library(recommenderlab)

source("Read Files of Movies.R")

movies <- ReadFiles()$Movies
moviesId <- movies$movieId

rating_matrix <- readRDS("rating_matrix.RDS")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Recomendadores
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
recomendadores <- readRDS("./Recomendadores/Recomendadores.RDS")


SVD_aprox <- recomendadores$SVD_aprox
IBCB <- recomendadores$IBCF
UBCF <- recomendadores$UBCF
POPULAR <- recomendadores$POPULAR
RANDOM <- recomendadores$RANDOM
ALS_implicit <- recomendadores$ALS_implicit



muestreador <- function(semilla, n=3, estrellas){
        set.seed(semilla)
        muestra <- movies[sample(nrow(movies), size=n),]
        muestra$userId <- c(611, 611, 611)
        muestra$rating <- estrellas
        return(muestra)
}


Recomendar_Pelicula <- function(recomendador, semilla, n, estrellas){
        
        prueba <- muestreador(semilla, n, estrellas)
        
        matriz <- matrix(NA, nrow = 1, ncol = 9724)
        
        matriz[1,which(moviesId==prueba$movieId[1])]<- prueba$rating[1]
        matriz[1,which(moviesId==prueba$movieId[2])]<- prueba$rating[2]
        matriz[1,which(moviesId==prueba$movieId[3])]<- prueba$rating[3]
        
        rating_prueba <- as(matriz, "realRatingMatrix")
        
        pred <- predict(recomendador, rating_prueba, n=5)
        
        p_ratings <- predict(recomendador, rating_prueba, type = "ratings")
        p_matrix_rating <- as(p_ratings, "matrix")
        
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
                filter( (movieId== id_movies[1]) | (movieId==id_movies[2]) |
                                (movieId== id_movies[3]) | (movieId== id_movies[4]) |
                                (movieId== id_movies[5])) %>%
                mutate(ratings = ratings_list) 
        
        top5_movies_ordenados <- top5_movies[order(match(top5_movies[,1], id_movies)),]
        
        print("En base a la elección y los Ratings de estas películas")
        print("")
        print(prueba[,c(2,3,5)])
        print("")
        print("Las recomendaciones para ud según sus gustos son:")
        top5_movies_ordenados
        
}
