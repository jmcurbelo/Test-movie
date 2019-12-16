library(recommenderlab)

ratin_matrix <- readRDS("rating_matrix.RDS")

SVD_aprox <- Recommender(ratin_matrix, method ="SVD", 
                         parameter =list(k=40, normalize = "Z-score", maxiter=200)
                         )
UBCF <- Recommender(ratin_matrix, method = "UBCF",
                    parameter = list(nn=70, method = "jaccard", normalize="Z-score"))

saveRDS(SVD_aprox, "SVD_aprox.RDS")
saveRDS(UBCF, "UBCF.RDS")
