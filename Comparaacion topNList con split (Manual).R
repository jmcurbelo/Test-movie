source("Create_Rating_Matrix.R")

library(recommenderlab)

ratin_matrix <- Create_Ratings_Matrix()

esq_eval <- evaluationScheme(ratin_matrix, method = "split",
                             train = 0.9, 
                             given = 20,
                             goodRating = 4)

random_item <- Recommender(getData(esq_eval, "train"), "RANDOM")
popular_items <- Recommender(getData(esq_eval, "train"), "POPULAR")
UBCF <- Recommender(getData(esq_eval, "train"), "UBCF")
Svd <- Recommender(getData(esq_eval, "train"), "SVD")
ALS <- Recommender(getData(esq_eval, "train"), "ALS")
ALS_implicit <- Recommender(getData(esq_eval, "train"), "ALS_implicit")
LIBMF <- Recommender(getData(esq_eval, "train"), "LIBMF")
SVDF <- Recommender(getData(esq_eval, "train"), "SVDF")
Rerecommend <- Recommender(getData(esq_eval, "train"), "RERECOMMEND")

p_random_items <- predict(random_item, getData(esq_eval, "known"), type="topNList", n=5)
p_popular_items <- predict(popular_items, getData(esq_eval, "known"), type="topNList", n=5)
p_UBCF <- predict(UBCF, getData(esq_eval, "known"), type="topNList", n=5)
p_Svd <- predict(Svd, getData(esq_eval, "known"), type="topNList", n=5)
p_ALS <- predict(ALS, getData(esq_eval, "known"), type="topNList", n=5)
p_ALS_implicit <- predict(ALS_implicit, getData(esq_eval, "known"), type="topNList", n=5)
p_LIBMF <- predict(LIBMF, getData(esq_eval, "known"), type="topNList", n=5)
p_SVDF <- predict(SVDF, getData(esq_eval, "known"), type="topNList", n=5)
p_Rerecommend <- predict(Rerecommend, getData(esq_eval, "known"), type="topNList", n=5)

print("Proceso completado")

error <- rbind(
        n_random_items = calcPredictionAccuracy(p_random_items, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_popular_items = calcPredictionAccuracy(p_popular_items, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_UBCF = calcPredictionAccuracy(p_UBCF, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_Svd = calcPredictionAccuracy(p_Svd, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_ALS = calcPredictionAccuracy(p_ALS, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_ALS_implicit = calcPredictionAccuracy(p_ALS_implicit, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_LIBMF = calcPredictionAccuracy(p_LIBMF, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_SVDF = calcPredictionAccuracy(p_SVDF, getData(esq_eval, "unknown"), goodRating=4, given=20),
        n_Rerecommend = calcPredictionAccuracy(p_Rerecommend, getData(esq_eval, "unknown"), goodRating=4, given=20)
)
error

##########################################
error_top10 <- error
##########################################
error_top5 <- error

error_top5
error_top10

saveRDS(error_top5, "error_top5.RDS")
saveRDS(error_top10, "error_top10.RDS")
