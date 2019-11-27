#' Exploracion de los datos. Realizamos tres histogramas para observar
#' el comportamiento de los ratings normalizados y sin normalizar

source("Create_Rating_Matrix.R")

ratings_matrix <- Create_Ratings_Matrix()

hist(getRatings(ratings_matrix), xlab = "Ratings",
     ylab = "Número de Ratings", main = "Ratings sin normalizar",
     col = "blue", border = "black")

hist(getRatings(normalize(ratings_matrix)), xlab = "Ratings",
     ylab = "Número de Ratings", main = "Ratings normalizados",
     col = "blue", border = "black")

hist(getRatings(normalize(ratings_matrix, method="Z-score")), xlab = "Ratings",
     ylab = "Número de Ratings", main = "Ratings normalizados con Z-score",
     col = "blue", border = "black")




