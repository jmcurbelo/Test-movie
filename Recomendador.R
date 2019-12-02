source("Create_Rating_Matrix.R")

library(recommenderlab)

rating_matrix <- Create_Ratings_Matrix()

esq_eval <- evaluationScheme(rating_matrix, method = "split",
                             train = 0.9, 
                             given = 20,
                             goodRating = 4)

ALS_implicit <- Recommender(getData(esq_eval, "train"), "ALS_implicit")
popular_items <- Recommender(getData(esq_eval, "train"), "POPULAR")

p_ALS_implicit <- predict(ALS_implicit, getData(esq_eval, "unknown")[1])
p_popular_items <- predict(popular_items, getData(esq_eval, "unknown")[1])

id_movies <- as.integer(as(p_ALS_implicit, "list")[[1]])
id_movies <- as.integer(as(p_popular_items, "list")[[1]])

movies <- ReadFiles()$Movies

# filtrando las peliculas recomendadas
library(dplyr)
top5_movies <- movies %>%
        select(movieId, title) %>%
        filter( (movieId== id_movies[1]) | (movieId==id_movies[2]) |
                (movieId== id_movies[3]) | (movieId== id_movies[4]) |
                (movieId== id_movies[5])) 

#ordenar de acuerdo al orden del recomendador
top5_movies[order(match(top5_movies[,1], id_movies)),]
