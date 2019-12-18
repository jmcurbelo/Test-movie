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
                    # fluidRow(
                    #     
                    #     box(
                    #         title = "Seleccione la primera película",
                    #         background = "blue",
                    #         selectInput(inputId = "pelicula1", label = "",
                    #                                     choices = titulos)
                    #     )
                    # ),
                    # 
                    # fluidRow(
                    #     box(
                    #         title = "Seleccione la segunda película",
                    #         background = "blue",
                    #         selectInput(inputId = "pelicula2", label = "",
                    #                                     choices = titulos)
                    #         
                    #     )
                    # ),
                    # 
                    # fluidRow(
                    #     box(
                    #         title = "Seleccione la tercera película",
                    #         background = "blue",
                    #         selectInput(inputId = "pelicula3", label = "",
                    #                     choices = titulos)
                    #     )
                    # )
                    fluidRow(
                        box(
                            title = "Seleccione las 5 películas de su preferencia",
                            background = "blue",
                            selectInput(inputId = "peliculas", label = "",
                                        choices = titulos),
                            actionButton(inputId = "add", label = "Agregar Película", icon = icon("calendar-plus"))
                        ),
                        box(
                            title = "Seleccione el algoritmo",
                            background = "blue",
                            radioButtons(inputId = "algoritmo", label = "",
                                               choices = list("UBCF"="UBCF","SVD"="SVD"))
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Su selección de películas",
                            background = "blue",
                            solidHeader = TRUE,
                            tableOutput("seleccion"),
                            actionButton(inputId = "borrar", label = "Borrar selección", icon = icon("backspace")),
                            actionButton(inputId = "borrarTodo", label = "Borrar todo", icon = icon("ban"))
                        )
                        
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "Recom",
                    # h2("Recomendaciones"),
                    # verbatimTextOutput("peliculas"),
                    fluidRow(
                        box(title = "Recomendaciones", status = "info",
                            solidHeader = TRUE,
                            tableOutput("tabla")
                            
                        )
                    )
                  
                    
                  
                    
            )
        )
    )
)


















