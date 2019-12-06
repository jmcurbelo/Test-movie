source("Create_Rating_Matrix.R")

library(recommenderlab)

##########################################################################
# creando la matriz de ratings
##########################################################################
rating_matrix <- Create_Ratings_Matrix()

##########################################################################
# leyendo los datos
##########################################################################
movies <- ReadFiles()$Movies
ratings <- ReadFiles()$Ratings

##########################################################################
# creando esquema de evaluacion para crear el recomendador
##########################################################################

# esq_eval <- evaluationScheme(rating_matrix, method = "split",
#                              train = 0.9, 
#                              given = 20,
#                              goodRating = 4)


##########################################################################
# creando los recomendadores
##########################################################################

# ALS_implicit <- Recommender(rating_matrix, "ALS_implicit")
#popular_items <- Recommender(getData(esq_eval, "train"), "POPULAR")
# IBCF <- Recommender(rating_matrix, "IBCF")
SVD_aprox <- Recommender(rating_matrix, "SVD")

##########################################################################
# creando el muestreador de peliculas y dandole un rating
##########################################################################

muestreador <- function(semilla, n=3, estrellas){
        set.seed(semilla)
        muestra <- movies[sample(nrow(movies), size=n),]
        muestra$userId <- c(611, 611, 611)
        muestra$rating <- estrellas
        return(muestra)
}

##########################################################################
# obteniendo los ID de las peliculas
##########################################################################

moviesId <- movies$movieId

##########################################################################
#' Seleccionando una muestra de n peliculas y asignandole ratings a cada una 
#' de las peliculas seleccionadas. Ademas se puede fijar una semilla para
#' que puedan ser repruducible los resultados
##########################################################################

prueba <- muestreador(1212,3,c(4,4,5)) #da error


print("Usted a seleccionado las siguientes películas con los siguientes ratings")
prueba[,c(2,3,5)]


##########################################################################
# creando la matriz de Ratings para el usuario
##########################################################################

matriz <- matrix(NA, nrow = 1, ncol = 9724)

##########################################################################
# asignando los ratings de las peliculas  a la matriz de Ratings
##########################################################################

matriz[1,which(moviesId==prueba$movieId[1])]<- prueba$rating[1]
matriz[1,which(moviesId==prueba$movieId[2])]<- prueba$rating[2]
matriz[1,which(moviesId==prueba$movieId[3])]<- prueba$rating[3]

##########################################################################
# convirtiendo la matriz de ratigns en una matriz tipo RealRating
##########################################################################
rating_prueba <- as(matriz, "realRatingMatrix")

##########################################################################
# prediciendo con un recomendador el top 5
##########################################################################

# p_popular_items <-  predict(popular_items, rating_prueba, n=5)
# p_ALS_implicit <- predict(ALS_implicit, rating_prueba, n=5)
# p_IBCF <- predict(IBCF, rating_prueba, n=5, type = "topNList")


p_ratings <- predict(SVD_aprox, rating_prueba, type = "ratings")
p_matrix_rating <- as(p_ratings, "matrix")


p_SVD <- predict(SVD_aprox, rating_prueba, n=5)

##########################################################################
# obteniendo los movieId de la prediccion
##########################################################################

#id_movies <- as.integer(as(p_popular_items, "list")[[1]])
# id_movies <- as.integer(as(p_IBCF, "list")[[1]])
id_movies <- as.integer(as(p_SVD, "list")[[1]])

# para agregar los ratings luego
ratings_list <- numeric()
ratings_list[1] <-  p_matrix_rating[1,which(id_movies[1]==moviesId)]
ratings_list[2] <-  p_matrix_rating[1,which(id_movies[2]==moviesId)]
ratings_list[3] <-  p_matrix_rating[1,which(id_movies[3]==moviesId)]
ratings_list[4] <-  p_matrix_rating[1,which(id_movies[4]==moviesId)]
ratings_list[5] <-  p_matrix_rating[1,which(id_movies[5]==moviesId)]




##########################################################################
# filtrando las peliculas recomendadas
##########################################################################

library(dplyr)
top5_movies <- movies %>%
        select(movieId, title, genres) %>%
        filter( (movieId== id_movies[1]) | (movieId==id_movies[2]) |
                        (movieId== id_movies[3]) | (movieId== id_movies[4]) |
                        (movieId== id_movies[5])) %>%
        mutate(ratings = ratings_list)

##########################################################################
#ordenar de acuerdo al orden del recomendador
##########################################################################

top5_movies_ordenados <- top5_movies[order(match(top5_movies[,1], id_movies)),]


##########################################################################
# Mostrando los resultados
##########################################################################
print("En base a la elección y los Ratings de estas películas")
prueba[,c(2,3,5)]

print("Las recomendaciones para ud según sus gustos son:")
top5_movies_ordenados



##########################################################################
# Encapsulando en una funcion
##########################################################################
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
        print(prueba[,c(2,3,5)])
        
        print("Las recomendaciones para ud según sus gustos son:")
        top5_movies_ordenados
        
}
