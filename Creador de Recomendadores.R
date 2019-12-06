library(recommenderlab)

rating_matrix <- readRDS("rating_matrix.RDS")

IBCF <- Recommender(rating_matrix, "IBCF")
UBCF <- Recommender(rating_matrix, "UBCF")
POPULAR <- Recommender(rating_matrix, "POPULAR")
RANDOM <- Recommender(rating_matrix, "RANDOM")
ALS_implicit <- Recommender(rating_matrix, "ALS_implicit")
SVD_aprox <- Recommender(rating_matrix, "SVD")

Recomendadores <- list(
        IBCF = IBCF,
        UBCF = UBCF,
        POPULAR = POPULAR,
        RANDOM = RANDOM,
        ALS_implicit = ALS_implicit,
        SVD_aprox = SVD_aprox
)

saveRDS(Recomendadores, "./Recomendadores/Recomendadores.RDS")