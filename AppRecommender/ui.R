library(shiny)
library(shinydashboard)


load(file = "./titulos.rda")

dashboardPage(
    dashboardHeader(title = "Recomendador"),
    dashboardSidebar(
        
        sidebarMenu(
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


# wellPanel(
#     selectInput(inputId = "pelicula1", label = "seleccionar pelicula",
#                 choices = c("op1","op2","op3")),
#     selectInput(inputId = "pelicula2", label = "seleccionar pelicula",
#                 choices = c("op1","op2","op3"))
# )
















# shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# ))
