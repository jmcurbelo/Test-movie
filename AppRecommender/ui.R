library(shiny)
library(shinydashboard)


load(file = "./titulos.rda")

dashboardPage(skin = "blue",
    dashboardHeader(title = "Recomendador"),
    dashboardSidebar(
        
        sidebarMenu(img(src="infomedia.png", width=232, height=40),
            menuItem("Seleccionar Película", tabName = "Sel_pel", icon = icon("list")),
            menuItem("Recomendaciones", tabName = "Recom", icon = icon("thumbs-up", lib = "glyphicon"))
        )
        
    
    ),
    dashboardBody(
        
        tabItems(
            # First tab content
            tabItem(tabName = "Sel_pel",
                    fluidRow(
                        
                        box(
                            title = "Seleccione la primera película",
                            background = "blue",
                            selectInput(inputId = "pelicula1", label = "",
                                                        choices = titulos)
                        )
                    ),
                    
                    fluidRow(
                        box(
                            title = "Seleccione la segunda película",
                            background = "blue",
                            selectInput(inputId = "pelicula2", label = "",
                                                        choices = titulos)
                            
                        )
                    ),
                    
                    fluidRow(
                        box(
                            title = "Seleccione la tercera película",
                            background = "blue",
                            selectInput(inputId = "pelicula3", label = "",
                                        choices = titulos)
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "Recom",
                    h2("Recomendaciones"),
                    verbatimTextOutput("peliculas")
                  
                    
                  
                    
            )
        )
    )
)

















