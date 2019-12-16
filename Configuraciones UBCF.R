library(recommenderlab)

rating_matrix <- readRDS("rating_matrix.RDS")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluacion del hiperparámetro nn 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

esq_eva_nn <- evaluationScheme(rating_matrix, method = "cross",
                               k=4,
                               given = 10,
                               goodRating = 4)

algoritmos_nn <- list(
        "UBCF20" = list(name = "UBCF", param = list(nn=20)),
        "UBCF25" = list(name = "UBCF", param = list(nn=25)),
        "UBCF30" = list(name = "UBCF", param = list(nn=30)),
        "UBCF35" = list(name = "UBCF", param = list(nn=35)),
        "UBCF40" = list(name = "UBCF", param = list(nn=40)),
        "UBCF45" = list(name = "UBCF", param = list(nn=45)),
        "UBCF50" = list(name = "UBCF", param = list(nn=50)),
        "UBCF55" = list(name = "UBCF", param = list(nn=55)),
        "UBCF60" = list(name = "UBCF", param = list(nn=60)),
        "UBCF65" = list(name = "UBCF", param = list(nn=65)),
        "UBCF70" = list(name = "UBCF", param = list(nn=70))
)

evaluacion_nn <- as(
        evaluate(esq_eva_nn, algoritmos_nn, type = "topNList", n=5),
        "list"
)

##################
# Precision
##################

cal_mean_prec <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][5]
        }
        return(acumulado/4)  
}

mean_prec_list <- function(lista){
        precisiones <- numeric()
        for (i in (1:11)) {
                precisiones[i] <- cal_mean_prec(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(precisiones)
        
}

###################
# Recall
###################
cal_mean_recall <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][6]
        }
        return(acumulado/4)  
}

mean_recall_list <- function(lista){
        recal <- numeric()
        for (i in (1:11)) {
                recal[i] <- cal_mean_recall(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(recal)
        
}

########################################
# Juntando los resultados en una matriz
########################################

prec_recall_nn <- matrix(
        c(mean_prec_list(evaluacion_nn), mean_recall_list(evaluacion_nn)),
        ncol = 2,
        dimnames = list(c("UBCF20","UBCF25","UBCF30","UBCF35",
                          "UBCF40","UBCF45","UBCF50","UBCF55",
                          "UBCF60","UBCF65","UBCF70"),
                        c("Precision","Recall"))
); prec_recall_nn


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluacion del hiperparámetro method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

esq_eva_method <- evaluationScheme(rating_matrix, method = "cross",
                               k=4,
                               given = 10,
                               goodRating = 4)

algoritmos_method <- list(
        "UBCF_cosine" = list(name = "UBCF", param = list(method = "cosine")),
        "UBCF_jaccard" = list(name = "UBCF", param = list(method = "jaccard"))
)

evaluacion_method <- as(
        evaluate(esq_eva_method, algoritmos_method, type = "topNList", n=5),
        "list"
)


##################
# Precision
##################

cal_mean_prec <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][5]
        }
        return(acumulado/4)  
}

mean_prec_list <- function(lista){
        precisiones <- numeric()
        for (i in (1:2)) {
                precisiones[i] <- cal_mean_prec(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(precisiones)
        
}

###################
# Recall
###################
cal_mean_recall <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][6]
        }
        return(acumulado/4)  
}

mean_recall_list <- function(lista){
        recal <- numeric()
        for (i in (1:2)) {
                recal[i] <- cal_mean_recall(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(recal)
        
}

########################################
# Juntando los resultados en una matriz
########################################

prec_recall_method <- matrix(
        c(mean_prec_list(evaluacion_method), mean_recall_list(evaluacion_method)),
        ncol = 2,
        dimnames = list(c("UBCF_cosine","UBCF_jaccard"),
                        c("Precision","Recall"))
); prec_recall_method


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluacion del hiperparámetro normalize
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

esq_eva_normalize <- evaluationScheme(rating_matrix, method = "cross",
                                   k=4,
                                   given = 10,
                                   goodRating = 4)

algoritmos_normalize <- list(
        "UBCF_center" = list(name = "UBCF", param = list(normalize = "center")),
        "UBCF_Z_score" = list(name = "UBCF", param = list(normalize = "Z-score"))
)

evaluacion_normalize <- as(
        evaluate(esq_eva_normalize, algoritmos_normalize, type = "topNList", n=5),
        "list"
)


##################
# Precision
##################

cal_mean_prec <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][5]
        }
        return(acumulado/4)  
}

mean_prec_list <- function(lista){
        precisiones <- numeric()
        for (i in (1:2)) {
                precisiones[i] <- cal_mean_prec(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(precisiones)
        
}

###################
# Recall
###################
cal_mean_recall <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][6]
        }
        return(acumulado/4)  
}

mean_recall_list <- function(lista){
        recal <- numeric()
        for (i in (1:2)) {
                recal[i] <- cal_mean_recall(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(recal)
        
}

########################################
# Juntando los resultados en una matriz
########################################

prec_recall_normalize <- matrix(
        c(mean_prec_list(evaluacion_normalize), mean_recall_list(evaluacion_normalize)),
        ncol = 2,
        dimnames = list(c("UBCF_center","UBCF_Z_score"),
                        c("Precision","Recall"))
); prec_recall_normalize

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Las pruebas individuales arrojaron que los mejores hiperparámetros
#' son nn=70, method="jaccard, normalize="Z-score"
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Probando diferentes configuraciones
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

esq_eva_configuraciones <- evaluationScheme(rating_matrix, method = "cross",
                                            k=4,
                                            given = 10,
                                            goodRating = 4)
algoritmos_configuraciones <- list(
        "UBCF1" = list(name = "UBCF", param = list(nn=70, method="jaccard",normalize = "center" )),
        "UBCF2" = list(name = "UBCF", param = list(nn=70, method="jaccard",normalize = "Z-score" )),
        "UBCF3" = list(name = "UBCF", param = list(nn=70, method="cosine",normalize = "center" )),
        "UBCF4" = list(name = "UBCF", param = list(nn=70, method="cosine",normalize = "Z-score" )),
        "UBCF5" = list(name = "UBCF", param = list(nn=60, method="jaccard",normalize = "center" )),
        "UBCF6" = list(name = "UBCF", param = list(nn=60, method="jaccard",normalize = "Z-score" )),
        "UBCF7" = list(name = "UBCF", param = list(nn=60, method="cosine",normalize = "center" )),
        "UBCF8" = list(name = "UBCF", param = list(nn=60, method="cosine",normalize = "Z-score" )),
        "UBCF9" = list(name = "UBCF", param = list(nn=80, method="jaccard",normalize = "center" )),
        "UBCF10" = list(name = "UBCF", param = list(nn=80, method="jaccard",normalize = "Z-score" )),
        "UBCF11" = list(name = "UBCF", param = list(nn=80, method="cosine",normalize = "center" )),
        "UBCF12" = list(name = "UBCF", param = list(nn=80, method="cosine",normalize = "Z-score" ))
)

evaluacion_configuraciones <- as(
        evaluate(esq_eva_configuraciones, algoritmos_configuraciones,
                 type = "topNList", n=5),
        "list"
)


##################
# Precision
##################

cal_mean_prec <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][5]
        }
        return(acumulado/4)  
}

mean_prec_list <- function(lista){
        precisiones <- numeric()
        for (i in (1:12)) {
                precisiones[i] <- cal_mean_prec(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(precisiones)
        
}

###################
# Recall
###################
cal_mean_recall <- function(lista){
        acumulado <- 0
        for (i in (1:4)) {
                acumulado <- acumulado + lista[[i]][6]
        }
        return(acumulado/4)  
}

mean_recall_list <- function(lista){
        recal <- numeric()
        for (i in (1:12)) {
                recal[i] <- cal_mean_recall(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(recal)
        
}


########################################
# Juntando los resultados en una matriz
########################################

prec_recall_configuraciones <- matrix(
        c(mean_prec_list(evaluacion_configuraciones), mean_recall_list(evaluacion_configuraciones)),
        ncol = 2,
        dimnames = list(
                c("UBCF1","UBCF2","UBCF3","UBCF4","UBCF5","UBCF6","UBCF7","UBCF8",
                  "UBCF9","UBCF10","UBCF11","UBCF12"),
                c("Precision","Recall")
        )
); prec_recall_configuraciones

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' La configuración que arroja mejores resultados es UBCF2 cuyos parámetros son
#' nn=70, method = "jaccard", normalize="Z-score"
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
