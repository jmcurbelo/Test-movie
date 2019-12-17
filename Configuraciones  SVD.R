library(recommenderlab)

ratin_matrix <- readRDS("rating_matrix.RDS")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluando el hiperparámetro k
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


esq_eval <- evaluationScheme(ratin_matrix, method = "cross",
                                           k=4, 
                                           given = 10,
                                           goodRating = 4)

algotithms <- list(
        "SVD10" = list(name = "SVD", param = list(k=10)),
        "SVD20" = list(name = "SVD", param = list(k=20)),
        "SVD30" = list(name = "SVD", param = list(k=30)),
        "SVD40" = list(name = "SVD", param = list(k=40)),
        "SVD50" = list(name = "SVD", param = list(k=50))
)

evaluacion <- evaluate(esq_eval, algotithms, type = "topNList", n=5)

evaluacion_list <- as(evaluacion, "list")

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
        for (i in (1:5)) {
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
        for (i in (1:5)) {
                recal[i] <- cal_mean_recall(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(recal)
        
}

prec_recall <- matrix(
        c(mean_prec_list(evaluacion_list),mean_recall_list(evaluacion_list)),
        ncol = 2,
        dimnames = list(c("SVD_k_10","SVD_k_20","SVD_k_30","SVD_k_40","SVD_k_50"),
                        c("Precision","Recall"))
);prec_recall



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluando el hiperparámetro maxiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

esq_eval_maxiter <- evaluationScheme(ratin_matrix, method = "cross",
                             k=4, 
                             given = 10,
                             goodRating = 4)

algotithms_maxiter <- list(
        "SVD100" = list(name = "SVD", param = list(maxiter=100)),
        "SVD200" = list(name = "SVD", param = list(maxiter=200)),
        "SVD300" = list(name = "SVD", param = list(maxiter=300)),
        "SVD400" = list(name = "SVD", param = list(maxiter=400)),
        "SVD500" = list(name = "SVD", param = list(maxiter=500))
)

evaluacion_maxiter <- evaluate(esq_eval_maxiter, algotithms_maxiter,
                               type = "topNList", n=5)

evaluacion_list_maxiter <- as(evaluacion_maxiter, "list")


prec_recall_maxiter <- matrix(
        c(mean_prec_list(evaluacion_list_maxiter),mean_recall_list(evaluacion_list_maxiter)),
        ncol = 2,
        dimnames = list(c("SVD_maxiter_100","SVD_maxiter_200","SVD_maxiter_300","SVD_maxiter_400","SVD_maxiter_500"),
                        c("Precision","Recall"))
);prec_recall_maxiter


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluando el hiperparámetro normalize
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

esq_eval_normalize <- evaluationScheme(ratin_matrix, method = "cross",
                                     k=4, 
                                     given = 10,
                                     goodRating = 4)

algotithms_normalize <- list(
        "SVD_center" = list(name = "SVD", param = list(normalize="center")),
        "SVD_Z_score" = list(name = "SVD", param = list(normalize="Z-score"))
        
)

evaluacion_normalize <- evaluate(esq_eval_normalize, algotithms_normalize,
                               type = "topNList", n=5)

evaluacion_list_normalize <- as(evaluacion_normalize, "list")

###############################################
# Modificando las funciones para dos algoritmos
###############################################

##################
# Precision
##################


mean_prec_list_normalize <- function(lista){
        precisiones <- numeric()
        for (i in (1:2)) {
                precisiones[i] <- cal_mean_prec(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(precisiones)
        
}


###################
# Recall
###################

mean_recall_list_normalize <- function(lista){
        recal <- numeric()
        for (i in (1:2)) {
                recal[i] <- cal_mean_recall(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(recal)
        
}


prec_recall_normalize <- matrix(
        c(mean_prec_list_normalize(evaluacion_list_normalize),mean_recall_list_normalize(evaluacion_list_normalize)),
        ncol = 2,
        dimnames = list(c("SVD_center","SVD_Z_score"),
                        c("Precision","Recall"))
);prec_recall_normalize

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Las configuraciones con mejores resultados son K=40, maxiter=300, 
#' normalize="Z-score".
#' 
#' A contunuacion probaremos el algoritmo por default vs el algoritmo 
#' con estas configuraciones de parámetros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

esquema_evaluacion <- evaluationScheme(ratin_matrix, method = "cross",
                                       k=4, 
                                       given = 10,
                                       goodRating = 4)
algoritmos <- list(
        "SVD_default" = list(name = "SVD", param = list(k=50)),
        "SVD_configurado" = list(name = "SVD", param = list(k=40, maxiter=300,normalize="Z-score"))
        
)

evaluacion_VS <- evaluate(esquema_evaluacion, algoritmos,
                                 type = "topNList", n=5)

evaluacion_VS_list <- as(evaluacion_VS, "list")

resultado <- matrix(
        c(mean_prec_list_normalize(evaluacion_VS_list), mean_recall_list_normalize(evaluacion_VS_list)),
        ncol = 2,
        dimnames = list(c("SVD_default","SDV_configurado"),
                        c("Precision","Recall"))
); resultado

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Otras confuguraciones
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

ratin_matrix <- readRDS("rating_matrix.RDS")

esq_eval <- evaluationScheme(ratin_matrix, method = "cross",
                             k=4,
                             given = 10,
                             goodRating = 4)
algoritmos <- list(
        "SVD1" = list(name = "SVD", param = list(k=40, maxiter=300, normalize="center")),
        "SVD2" = list(name = "SVD", param = list(k=40, maxiter=300, normalize="Z-score")),
        "SVD3" = list(name = "SVD", param = list(k=50, maxiter=300, normalize="center")),
        "SVD4" = list(name = "SVD", param = list(k=50, maxiter=300, normalize="Z-score")),
        "SVD5" = list(name = "SVD", param = list(k=60, maxiter=300, normalize="center")),
        "SVD6" = list(name = "SVD", param = list(k=60, maxiter=300, normalize="Z-score")),
        "SVD7" = list(name = "SVD", param = list(k=70, maxiter=300, normalize="center")),
        "SVD8" = list(name = "SVD", param = list(k=70, maxiter=300, normalize="Z-score")),
        "SVD9" = list(name = "SVD", param = list(k=40, maxiter=200, normalize="center")),
        "SVD10" = list(name = "SVD", param = list(k=40, maxiter=200, normalize="Z-score"))
)

evaluacion <- as(
        evaluate(esq_eval, algoritmos, type = "topNList", n=5),
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
        for (i in (1:10)) {
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
        for (i in (1:10)) {
                recal[i] <- cal_mean_recall(getConfusionMatrix(lista[[i]]))
                
        }
        
        return(recal)
        
}

prec_recall <- matrix(
        c(mean_prec_list(evaluacion), mean_prec_list(evaluacion)),
        ncol = 2,
        dimnames = list(
                c("SVD1","SVD2","SVD3","SVD4","SVD5","SVD6","SVD7","SVD8",
                  "SVD9","SVD10"), 
                c("Precision", "Recall")
        )
); prec_recall

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Los mejores resultados se alcanzan con k=40, normalize="Z-score" y 
#' maxiter=200/300 (identicos resultados)
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~