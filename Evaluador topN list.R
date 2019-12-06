set.seed(2019)

source("Create_Rating_Matrix.R")

library(recommenderlab)

ratin_matrix <- Create_Ratings_Matrix()


#################################################################
# Esquema cross validation
#################################################################

###############
# Given 2
###############

esq_crossVal_given2 <- evaluationScheme(ratin_matrix, method = "cross",
                             k=4, 
                             given = -2,
                             goodRating = 4)

algorithms <- list(
        "IBCF" = list(name = "IBCF", param = NULL),
        "UBCF" = list(name = "UBCF", param = list(nn=50)),
        "popular items" = list(name = "POPULAR", param = NULL),
        "random items" = list(name = "RANDOM", param = NULL),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "SVD approximation" = list(name = "SVD", param = list(k=50))
        
)

crossVal_given2 <- evaluate(esq_crossVal_given2, algorithms, type = "topNList",
                    n=c(1,3,5,10,15,20))

saveRDS(crossVal_given2,"./Esquemas/crossVal_given2.RDS")


###############
# Given 5
###############

esq_crossVal_given5 <- evaluationScheme(ratin_matrix, method = "cross",
                                        k=4, 
                                        given = -5,
                                        goodRating = 4)

algorithms <- list(
        "IBCF" = list(name = "IBCF", param = NULL),
        "UBCF" = list(name = "UBCF", param = list(nn=50)),
        "popular items" = list(name = "POPULAR", param = NULL),
        "random items" = list(name = "RANDOM", param = NULL),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "SVD approximation" = list(name = "SVD", param = list(k=50))
        
)

crossVal_given5 <- evaluate(esq_crossVal_given5, algorithms, type = "topNList",
                            n=c(1,3,5,10,15,20))

saveRDS(crossVal_given5,"./Esquemas/crossVal_given5.RDS")



###############
# Given 10
###############

esq_crossVal_given10 <- evaluationScheme(ratin_matrix, method = "cross",
                                        k=4, 
                                        given = -10,
                                        goodRating = 4)

algorithms <- list(
        "IBCF" = list(name = "IBCF", param = NULL),
        "UBCF" = list(name = "UBCF", param = list(nn=50)),
        "popular items" = list(name = "POPULAR", param = NULL),
        "random items" = list(name = "RANDOM", param = NULL),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "SVD approximation" = list(name = "SVD", param = list(k=50))
        
)

crossVal_given10 <- evaluate(esq_crossVal_given10, algorithms, type = "topNList",
                            n=c(1,3,5,10,15,20))

saveRDS(crossVal_given10,"./Esquemas/crossVal_given10.RDS")


###############
# All but 2
###############

esq_crossVal_all_but2 <- evaluationScheme(ratin_matrix, method = "cross",
                                        k=4, 
                                        given = 2,
                                        goodRating = 4)

algorithms <- list(
        "IBCF" = list(name = "IBCF", param = NULL),
        "UBCF" = list(name = "UBCF", param = list(nn=50)),
        "popular items" = list(name = "POPULAR", param = NULL),
        "random items" = list(name = "RANDOM", param = NULL),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "SVD approximation" = list(name = "SVD", param = list(k=50))
        
)

crossVal_all_but2 <- evaluate(esq_crossVal_all_but2, algorithms, type = "topNList",
                            n=c(1,3,5,10,15,20))

saveRDS(crossVal_all_but2,"./Esquemas/crossVal_all_but2.RDS")


###############
# All but 5
###############

esq_crossVal_all_but5 <- evaluationScheme(ratin_matrix, method = "cross",
                                          k=4, 
                                          given = 5,
                                          goodRating = 4)

algorithms <- list(
        "IBCF" = list(name = "IBCF", param = NULL),
        "UBCF" = list(name = "UBCF", param = list(nn=50)),
        "popular items" = list(name = "POPULAR", param = NULL),
        "random items" = list(name = "RANDOM", param = NULL),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "SVD approximation" = list(name = "SVD", param = list(k=50))
        
)

crossVal_all_but5 <- evaluate(esq_crossVal_all_but5, algorithms, type = "topNList",
                              n=c(1,3,5,10,15,20))

saveRDS(crossVal_all_but5,"./Esquemas/crossVal_all_but5.RDS")


###############
# All but 10
###############

esq_crossVal_all_but10 <- evaluationScheme(ratin_matrix, method = "cross",
                                          k=4, 
                                          given = 10,
                                          goodRating = 4)

algorithms <- list(
        "IBCF" = list(name = "IBCF", param = NULL),
        "UBCF" = list(name = "UBCF", param = list(nn=50)),
        "popular items" = list(name = "POPULAR", param = NULL),
        "random items" = list(name = "RANDOM", param = NULL),
        "ALS_implicit" = list(name = "ALS_implicit", param = NULL),
        "SVD approximation" = list(name = "SVD", param = list(k=50))
        
)

crossVal_all_but10 <- evaluate(esq_crossVal_all_but10, algorithms, type = "topNList",
                              n=c(1,3,5,10,15,20))

saveRDS(crossVal_all_but10,"./Esquemas/crossVal_all_but10.RDS")



