library(shiny)
library(shinythemes)
library(httr)
library(jpeg)
library(png)
library(magick)
library(unglue)
library(recommenderlab)

# Sys.setlocale("LC_ALL", 'es_ES.UTF-8')
# setwd("/Users/rubenadad/Documents/OneDrive/Documentos/Infomedia servicios/Demo recomendaciones/ml-latest-small")

# Función para leer atributos de las películas usando un API de IMDB
getmovie <- function(movie,year) {
  # Búsqueda por nombre y año de la película
  res <- GET("https://movie-database-imdb-alternative.p.rapidapi.com/",
             query=list("s"=movie,"y"=year,
                        "r"="json"
             ), 
             add_headers(
               "X-RapidAPI-Key"="77c301658dmsh580f13e236682e0p1c5c69jsn6dbc7ce99cfa"
             )
  )
  contenido <- content(res, "parsed")
  # Valida que se haya obtenido algún resultado, de lo contrario se termina la función
  # regresando un mensaje de error
  if (contenido$Response == "False") {
    return(paste(movie,"película no encontrada"))}
  # Busca el título exacto dentro del resultado que puede traer varias películas
  exacto <- lapply(contenido$Search, function(x) grep(movie, x))
  indice <- which(sapply(exacto, function(x) length(x) > 0))
  cantidad <- length(indice)
  #print(cantidad)
  # Si se encontró la película se asignan algunos atributos a un dataframe
  if (cantidad > 0) {
    j <- 1
    contenido_df <- data.frame("Title" = contenido$Search[indice[j]][[1]]$Title,
                               "Year" = contenido$Search[indice[j]][[1]]$Year,
                               "imdbID" = contenido$Search[indice[j]][[1]]$imdbID,
                               "Poster" = contenido$Search[indice[j]][[1]]$Poster,
                               stringsAsFactors = FALSE)
    j <- j + 1
    # Si se encontraron varios casos con el mismo título se busca alguno que tenga
    # el poster de la película.
    while (contenido_df$Poster == "N/A" & cantidad >= j) {
      contenido_df <- data.frame("Title" = contenido$Search[indice[j]][[1]]$Title,
                               "Year" = contenido$Search[indice[j]][[1]]$Year,
                               "imdbID" = contenido$Search[indice[j]][[1]]$imdbID,
                               "Poster" = contenido$Search[indice[j]][[1]]$Poster,
                               stringsAsFactors = FALSE)
      j <- j + 1
    }
    # Se verifica que se haya encontrado la imagen de la película, de lo contrario
    # se termina la función con un mensaje de error
    if (contenido_df$Poster == "N/A") {
      return(paste(contenido_df$Title,"imagen no encontrada"))}
    # Se busca usando el imdbID para obtener atributos adicionales
    res <- GET("https://movie-database-imdb-alternative.p.rapidapi.com/",
               query=list("i"=contenido_df$imdbID,
                          "r"="json"
               ), 
               add_headers(
                 "X-RapidAPI-Key"="77c301658dmsh580f13e236682e0p1c5c69jsn6dbc7ce99cfa"
               )
    )
    contenido <- content(res, "parsed")
    contenido_df$Genre <- contenido$Genre
    contenido_df$Director <- contenido$Director
    contenido_df$Actors <- contenido$Actors
    contenido_df[contenido$Ratings[1][[1]]$Source] <- contenido$Ratings[1][[1]]$Value
    contenido_df[contenido$Ratings[2][[1]]$Source] <- contenido$Ratings[2][[1]]$Value
    contenido_df[contenido$Ratings[3][[1]]$Source] <- contenido$Ratings[3][[1]]$Value
    mensaje <- tryCatch({img <- image_read(contenido_df$Poster)}, 
                   error = function(cond) {
                     return(paste("error al leer imagen", contenido_df$Title, contenido_df$Poster))
                     })
  } else {
    return(paste(movie,"película no encontrada"))
  }
  
  return(list("tabla" = contenido_df, "imagen" = img))
}

# Se lee el archivo con las películas obtenidas de "grouplens"
# movies <- read.csv("movies.csv", stringsAsFactors = FALSE)
movies <- read.csv("./movies.csv", header = TRUE, stringsAsFactors = FALSE)

# Se lee el archivo con las calificaciones de películas obtenidas de "grouplens"
ratings <- read.csv("./ratings.csv", stringsAsFactors = FALSE)
# ratings <- read.csv("ratings.csv", stringsAsFactors = FALSE)
ratings <- ratings[,1:3]
# Se convierte a una matriz en el formato requerido por el paquete recommenderlab
# ratings_matrix <- as(ratings,"realRatingMatrix")
# Información general de la matriz
# image(ratings_matrix[1:10,1:10])
# head(getRatingMatrix(ratings_matrix))
# head(getRatings(ratings_matrix))
# head(as(ratings_matrix, "list"))
# Se normalizan los valores de las calificaciones
# rc <- normalize(ratings_matrix)
# image(rc[1:10,1:10])

# Se entrena un modelo usando user-based collaborative filtering
# rec <- Recommender(ratings_matrix, method = "UBCF") 
# print(getModel(rec))

load(file = "./UBCF.rda")


ui = fluidPage(
  title = "Recomendaciones para películas",
  fluidRow(
    column(width = 5),
    column(width = 3, align = "center", 
           wellPanel(actionButton("peliculas", "Listar películas"))
    ),
    column(width = 4, align = "center",
           wellPanel(actionButton("recomendar", "Recomendar películas"))
    )
  ),
  fluidRow(
    tabsetPanel(id = "tabs",
      tabPanel("Inicio",
        column(width = 12,
               imageOutput("image01", width = "800px", height = "1000px",
                           click = "image_click",
               )
        )
      ),
      tabPanel("Selección",
        column(width = 12, 
               h4("Películas seleccionadas", align = "center"),
               DT::dataTableOutput('view', width="100%")
        )
      )
    )
  )
)

server = function(input, output, session) {
  rv <- reactiveValues(
    img_idx = vector(),
    pelis_sel = data.frame(),
    pelis_rec = data.frame(),
    pelis_df = data.frame()
  )
  
  
  observeEvent(input$peliculas, {
    
    
    
    
    withProgress(message = "Listando Películas", style = "notification", value = 0.1, {
      Sys.sleep(0.25)
      
      
      
      
      
      
      # Se cierra la pestaña de recomendaciones si estuviera abierta
      removeTab(inputId = "tabs", target = "Recomendaciones")
      # Se obtiene una lista de 50 películas seleccionadas al azar
      samp50 <- movies[sample(nrow(movies), 50), ]
      # Se separa el título de la película del año de producción
      pattern = c("{titulo} ({año=\\d{4}})")
      lista_new <- cbind(unglue_data(samp50$title, pattern), samp50$movieId)
      colnames(lista_new)[3] <- "movieId"
      lista_new$titulo <- gsub("\\s*\\([^\\)]+\\)","",as.character(lista_new$titulo))
      lista_new$titulo <- gsub(",.*$", "", lista_new$titulo)
      
      # Se buscan 20 películas en IMDB. Sus atributos se guardan en el dataframe pelis_df
      rv$pelis_df <- data.frame()
      i <- 1
      titulos <- 1
      while (titulos <= 20) {
        respuesta <- getmovie(lista_new$titulo[i], lista_new$año[i])
        if (is.list(respuesta)) {
          respuesta$tabla$movieId <- lista_new$movieId[i]
          rv$pelis_df <- dplyr::bind_rows(rv$pelis_df, respuesta$tabla)
          titulos <- titulos + 1
          
        }
        else {
          print(respuesta)
        }
        i <- i + 1
        
        
      }
      
      
      incProgress(0.5)
      
      
      # Se concatenan las 20 imágenes en un cuadro de 5x4
      imagenes <- image_scale(image_read(rv$pelis_df$Poster), geometry_size_pixels(136,200))
      print(length(imagenes))
      i_1 <- image_append(imagenes[1:5])
      i_2 <- image_append(imagenes[6:10])
      i_3 <- image_append(imagenes[11:15])
      i_4 <- image_append(imagenes[16:20])
      all_images <- image_append(c(i_1, i_2, i_3, i_4), stack = TRUE)
      
      
      
      incProgress(0.2)
      
      
      output$image01 <- renderImage({
        # Se obtiene el ancho y alto de la imagen
        width  <<- session$clientData$output_image01_width
        height <<- session$clientData$output_image01_height
        
        # Se genera un archivo temporal con la imagen
        outfile <- tempfile(fileext = ".png")
        
        png(outfile, width=width, height=height)
        plot(all_images)
        dev.off()
        
        
        
        
        incProgress(0.2)
        
        
        
        # Lista con información de la imagen
        list(
          src = outfile,
          contentType = "image/png",
          width = width,
          height = height
        )
      })
      
      
      
      
      setProgress(1)
      
      
      
      
      
      
    })
    
      
      
      
      

      
  })
      
      

    
  
  

  observeEvent(input$image_click, {
    
    showNotification(paste("Película seleccionada"), duration = 1.5)
    
    # calcula coordenadas de la imagen a la que se le da click
    x_margen <- 60
    y_margen <- 90
    x_poster <- (width - 2*x_margen)/5
    y_poster <- (height - 2*y_margen)/4
    x_click <-  input$image_click$x
    y_click <-  input$image_click$y
    print(paste(x_click, y_click))
    # calcula el renglón y columna correspondientes a la imagen a la que se dio click
    img_sel <- c(ceiling((x_click-x_margen)/x_poster), ceiling((y_click-y_margen)/y_poster))
    print(img_sel)
    # se convierte el renglón y columna al indice que corresponde al vector de las imágenes
    tmp_idx <- img_sel[1] + (img_sel[2]-1)*5
    rv$img_idx <- append(rv$img_idx, tmp_idx)
    print(rv$img_idx)
    rv$pelis_sel <- rv$pelis_df[rv$img_idx,-4]
    print(rv$pelis_sel)
  
    output$view <- DT::renderDataTable(rv$pelis_sel, options = list(searching = FALSE))
  })
  
  
  
  
  
  
  
  observeEvent(input$recomendar, {
    withProgress(message = "Buscando las Recomendaciones", style = "notification", value = 0.1, {
      Sys.sleep(0.25)
      
      
      
      new_ratings <- cbind(99999, rv$pelis_sel$movieId, runif(nrow(rv$pelis_sel), 4.0, 5.0))
      colnames(new_ratings) <- colnames(ratings)
      #print(new_ratings)
      ratings <- rbind(ratings, new_ratings)
      ratings_matrix <- as(ratings,"realRatingMatrix")
      recom <- predict(UBCF, ratings_matrix[nrow(ratings_matrix)], n=20)
      recom_id <- as(recom, "list")[[1]]
      #print(recom_id)
      rv$pelis_rec <- movies[movies$movieId %in% recom_id,]
      
      
      
      incProgress(0.3)
      
      
      #print(rv$pelis_rec)
      
      # Se separa el título de la película del año de producción
      pattern = c("{titulo} ({año=\\d{4}})")
      lista_new <- cbind(unglue_data(rv$pelis_rec$title, pattern), rv$pelis_rec$movieId)
      colnames(lista_new)[3] <- "movieId"
      lista_new$titulo <- gsub("\\s*\\([^\\)]+\\)","",as.character(lista_new$titulo))
      lista_new$titulo <- gsub(",.*$", "", lista_new$titulo)
      
      
      incProgress(0.1)
      
      
      # Se buscan las películas recomendadas en IMDB.
      rv$pelis_df <<- data.frame()
      for (i in 1:nrow(rv$pelis_rec)) {
        respuesta <- getmovie(lista_new$titulo[i], lista_new$año[i])
        if (is.list(respuesta)) {
          respuesta$tabla$movieId <- lista_new$movieId[i]
          rv$pelis_df <<- dplyr::bind_rows(rv$pelis_df, respuesta$tabla)
        }
        else {
          print(respuesta)
        }
      }
      rv$pelis_df <- rv$pelis_df[,-4]
      
      
      
      incProgress(0.4)
      
      appendTab(inputId = "tabs", 
                tabPanel("Recomendaciones", 
                         column(width = 12,
                                h4("Películas recomendadas", align = "center"),
                                DT::dataTableOutput("pelis_rec"),
                                actionButton("new_recom", label = "Nueva recomendación", class = "color_btn")
                         )
                )
      )
      # Muestra la tabla con las películas recomendadas
      output$pelis_rec <- DT::renderDataTable({
        DT::datatable(rv$pelis_df, rownames= FALSE, options = list(searching = FALSE, pageLength = 20))
      }, server = TRUE) 
      
      
      
      
      setProgress(1)
      
      
      
    })
    
    

  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$new_recom, {
    
    withProgress(message = "Buscando las Recomendaciones", style = "notification", value = 0.1, {
      Sys.sleep(0.25)
      
      
      
      removeTab(inputId = "tabs", target = "Recomendaciones")
      
      rows_sel <- input$pelis_rec_rows_selected
      rv$pelis_sel <- rbind(rv$pelis_sel, rv$pelis_df[rows_sel,])
      #print(rv$pelis_sel)
      new_ratings <- cbind(99999, rv$pelis_sel$movieId, runif(nrow(rv$pelis_sel), 4.0, 5.0))
      colnames(new_ratings) <- colnames(ratings)
      print(new_ratings)
      ratings <- rbind(ratings, new_ratings)
      ratings_matrix <- as(ratings,"realRatingMatrix")
      
      
      incProgress(0.2)
      
      
      recom <- predict(UBCF, ratings_matrix[nrow(ratings_matrix)], n=20)
      recom_id <- as(recom, "list")[[1]]
      
      
      incProgress(0.2)
      
      
      #print(recom_id)
      rv$pelis_rec <- movies[movies$movieId %in% recom_id,]
      #print(rv$pelis_rec)
      
      # Se separa el título de la película del año de producción
      pattern = c("{titulo} ({año=\\d{4}})")
      lista_new <- cbind(unglue_data(rv$pelis_rec$title, pattern), rv$pelis_rec$movieId)
      colnames(lista_new)[3] <- "movieId"
      lista_new$titulo <- gsub("\\s*\\([^\\)]+\\)","",as.character(lista_new$titulo))
      lista_new$titulo <- gsub(",.*$", "", lista_new$titulo)
      
      
      incProgress(0.1)
      
      
      # Se buscan las películas recomendadas en IMDB.
      rv$pelis_df <<- data.frame()
      for (i in 1:nrow(rv$pelis_rec)) {
        respuesta <- getmovie(lista_new$titulo[i], lista_new$año[i])
        if (is.list(respuesta)) {
          respuesta$tabla$movieId <- lista_new$movieId[i]
          rv$pelis_df <<- dplyr::bind_rows(rv$pelis_df, respuesta$tabla)
        }
        else {
          print(respuesta)
        }
      }
      rv$pelis_df <- rv$pelis_df[,-4]
      
      
      incProgress(0.4)
      
      
      appendTab(inputId = "tabs", 
                tabPanel("Recomendaciones", 
                         column(width = 12,
                                h4("Películas recomendadas", align = "center"),
                                DT::dataTableOutput("pelis_rec"),
                                actionButton("new_recom", label = "Nueva recomendación", class = "color_btn")
                         )
                )
      )
      # Muestra la tabla con las películas recomendadas
      output$pelis_rec <- DT::renderDataTable({
        DT::datatable(rv$pelis_df, rownames= FALSE, options = list(searching = FALSE, pageLength = 20))
      }, server = TRUE)
      
      
      
      setProgress(1)
      
      
    })
    
    

  })
}

shinyApp(ui, server)