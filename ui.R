## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("Recommendation By Genre", tabName = "genre", icon = icon("film")),
              menuItem("Recommendation By Rating", tabName = "rating", icon = icon("star"))
            )
          ),
          
          dashboardBody(includeCSS("css/books.css"),
          
            tabItems(
              
              #First tab
              tabItem(tabName = "genre",
                      fluidRow(
                        box(width = 12, title = "Step 1: Rate as many books as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                            h2("Test Test"),
                            selectInput("genreDropdown","Select Genre",choices = genre_list )
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover books you might like",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btnGenre", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("resultsGenre")
                        )
                      )  
                      
                      

                      
              ),
              
              #Second Tag
              tabItem(tabName = "rating",
                      fluidRow(
                        box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                            div(class = "rateitems",
                                uiOutput('ratings')
                            )
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("results")
                        )
                      )   
                
              )
              
              
            )
          
          )


    )
) 