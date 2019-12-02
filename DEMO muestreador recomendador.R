source("Create_Rating_Matrix.R")

# creando la matriz de ratings
rating_matrix <- Create_Ratings_Matrix()

# leyendo los datos
movies <- ReadFiles()$Movies
ratings <- ReadFiles()$Ratings

# creando esquema de evaluacion para crear el recomendador
esq_eval <- evaluationScheme(rating_matrix, method = "split",
                             train = 0.9, 
                             given = 20,
                             goodRating = 4)


# creando los recomendadores
# ALS_implicit <- Recommender(getData(esq_eval, "train"), "ALS_implicit")
popular_items <- Recommender(getData(esq_eval, "train"), "POPULAR")


# creando el muestreador de peliculas y dandole un rating
muestreador <- function(semilla, n=3, estrellas){
        set.seed(semilla)
        muestra <- movies[sample(nrow(movies), size=n),]
        muestra$userId <- c(611, 611, 611)
        muestra$rating <- estrellas
        return(muestra)
}

# obteniendo una muestra
prueba <- muestreador(1234,3,c(4.5,5,4))

print("Usted a seleccionado la siguientes películas con los siguientes ratings")
prueba[,c(1,2,5)]


# obteniendo los ID de las peliculas
moviesId <- movies$movieId

# creando la matriz con NA
matriz <- matrix(NA, nrow = 1, ncol = 9724)

# asignando los ratings introducidos a la matriz
matriz[1,which(moviesId==prueba$movieId[1])]<- prueba$rating[1]
matriz[1,which(moviesId==prueba$movieId[2])]<- prueba$rating[2]
matriz[1,which(moviesId==prueba$movieId[3])]<- prueba$rating[3]

# convirtiendo los ratigns en una matriz tipo RealRating
rating_prueba <- as(matriz, "realRatingMatrix")


# prediciendo con un recomendador el top 5
p_popular_items <-  predict(popular_items, rating_prueba, n=5)

# obteniendo los movieId de la prediccion
id_movies <- as.integer(as(p_popular_items, "list")[[1]])


# filtrando las peliculas recomendadas
library(dplyr)
top5_movies <- movies %>%
        select(movieId, title) %>%
        filter( (movieId== id_movies[1]) | (movieId==id_movies[2]) |
                        (movieId== id_movies[3]) | (movieId== id_movies[4]) |
                        (movieId== id_movies[5])) 

#ordenar de acuerdo al orden del recomendador
top5_movies_ordenados <- top5_movies[order(match(top5_movies[,1], id_movies)),]




