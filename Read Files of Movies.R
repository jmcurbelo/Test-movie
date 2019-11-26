#' Lee los archivos necesarios y devuelve una lista con cada uno de los
#' ficheros links, movies, ratings y tags

ReadFiles <- function(){
        links <- read.csv("./Data/links.csv", header = TRUE)
        movies <- read.csv("./Data/movies.csv", header = TRUE)
        ratings <- read.csv("./Data/ratings.csv", header = TRUE)
        tags <- read.csv("./Data/tags.csv", header = TRUE)
        
        return(list(Links=links, Movies=movies, Ratings=ratings, Tags=tags))
}


