library(recommenderlab)

all_but10 <- readRDS("./Esquemas/crossVal_all_but10.RDS")
all_but5 <- readRDS("./Esquemas/crossVal_all_but5.RDS")
all_but2 <- readRDS("./Esquemas/crossVal_all_but2.RDS")

given_10 <- readRDS("./Esquemas/crossVal_given10.RDS")
given_5 <- readRDS("./Esquemas/crossVal_given5.RDS")
given_2 <- readRDS("./Esquemas/crossVal_given2.RDS")



plot(all_but10, annotate = TRUE, legend = "topleft")
plot(all_but10, "prec/rec", annotate = TRUE, legend = "bottomright")

plot(all_but5, annotate = TRUE, legend = "topleft")
plot(all_but5, "prec/rec", annotate = TRUE, legend = "bottomright")

plot(all_but2, annotate = TRUE, legend = "topleft")
plot(all_but2, "prec/rec", annotate = TRUE, legend = "bottomright")

plot(given_10, annotate = TRUE, legend = "topleft")
plot(given_10, "prec/rec", annotate = TRUE, legend = "bottomright")

plot(given_5, annotate = TRUE, legend = "topleft")
plot(given_5, "prec/rec", annotate = TRUE, legend = "bottomright")

plot(given_2, annotate = TRUE, legend = "topleft")
plot(given_2, "prec/rec", annotate = TRUE, legend = "bottomright")





