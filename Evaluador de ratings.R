set.seed(2019)

library(recommenderlab)

ratin_matrix <- readRDS("rating_matrix.RDS")

esq_eval <- evaluationScheme(ratin_matrix, method = "split",
                             train = 0.9,
                             given = -5,
                             goodRating = 4)

algorithms <- list(
        "random items" = list(name = "RANDOM", param = NULL),
        "popular items" = list(name = "POPULAR", param = NULL),
        "user-based CF" = list(name = "UBCF", param = list(nn=50)),
        "SVD approximation" = list(name = "SVD", param = list(k=50)),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "IBCF" = list(name = "IBCF", param = NULL)
)

results <- evaluate(esq_eval, algorithms, type = "ratings")

saveRDS(results, "./Esquemas/Ratings.RDS")

lista_resultados <- as(results, "list")

RANDOM = getConfusionMatrix(lista_resultados[[1]])
POPULAR = getConfusionMatrix(lista_resultados[[2]])
UBCF = getConfusionMatrix(lista_resultados[[3]])
SVD_aprox = getConfusionMatrix(lista_resultados[[4]])
ALS = getConfusionMatrix(lista_resultados[[5]])
ALS_implicit = getConfusionMatrix(lista_resultados[[6]])
LIBMF = getConfusionMatrix(lista_resultados[[7]])
SVDF = getConfusionMatrix(lista_resultados[[8]])
RERECOMMEND = getConfusionMatrix(lista_resultados[[9]])
IBCF = getConfusionMatrix(lista_resultados[[10]])

error <- rbind(
        RANDOM = c(RANDOM[[1]][1], RANDOM[[1]][2], RANDOM[[1]][3]),
        POPULAR = c(POPULAR[[1]][1], POPULAR[[1]][2], POPULAR[[1]][3]), 
        UBCF = c(UBCF[[1]][1], UBCF[[1]][2], UBCF[[1]][3]),
        SVD_aprox = c(SVD_aprox[[1]][1], SVD_aprox[[1]][2], SVD_aprox[[1]][3]),
        ALS = c(ALS[[1]][1], ALS[[1]][2], ALS[[1]][3]),
        ALS_implicit = c(ALS_implicit[[1]][1], ALS_implicit[[1]][2], ALS_implicit[[1]][3]),
        LIBMF = c(LIBMF[[1]][1], LIBMF[[1]][2], LIBMF[[1]][3]),
        SVDF = c(SVDF[[1]][1], SVDF[[1]][2], SVDF[[1]][3]),
        RERECOMMEND = c(RERECOMMEND[[1]][1], RERECOMMEND[[1]][2], RERECOMMEND[[1]][3]),
        IBCF = c(IBCF[[1]][1], IBCF[[1]][2], IBCF[[1]][3])
        
)

colnames(error)<- c("RMSE", "MSE", "MAE")

error
