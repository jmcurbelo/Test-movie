source("Create_Rating_Matrix.R")

library(recommenderlab)

ratin_matrix <- Create_Ratings_Matrix()

esq_eval <- evaluationScheme(ratin_matrix, method = "split",
                             train = 0.9,
                             given = 20,
                             goodRating = 4)

algorithms <- list(
        "random items" = list(name = "RANDOM", param = NULL),
        "popular items" = list(name = "POPULAR", param = NULL),
        "user-based CF" = list(name = "UBCF", param = list(nn=50)),
        "SVD approximation" = list(name = "SVD", param = list(k=50)),
        "ALS" = list(name = "ALS", param = NULL),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "LIBMF" = list(name = "LIBMF", param = NULL),
        "SVDF" = list(name = "SVDF", param = list(k=50)),
        "RERECOMMEND" = list(name = "RERECOMMEND", param = NULL)
)

results <- evaluate(esq_eval, algorithms, type = "topNList",
                    n=c(5,10))

print("Proceso completado")

results

saveRDS(results, file = "topN_split.RDS")

names(results)

results$`random items`

plot(results, annotate=c(1,3), legend="bottomright")
plot(results, "prec/rec", annotate=3, legend="topleft")

getConfusionMatrix(results$`ALS_implicit`)
