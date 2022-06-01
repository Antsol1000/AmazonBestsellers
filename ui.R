library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(magrittr)
library(billboarder)
library(reshape2)

genres_vector <- c("Young Adult" = "Young Adult",
                   "Science Fiction" = "Science Fiction" ,
                   "Romance" = "Romance" ,
                   "Paranormal" = "Paranormal",
                   "Novels" = "Novels",
                   "Nonfiction" = "Nonfiction" ,
                   "Mystery" = "Mystery",
                   "Literature" = "Literature",
                   "Historical Fiction" = "Historical Fiction",
                   "Historical" = "Historical",
                   "Fiction" = "Fiction",
                   "Fantasy" = "Fantasy" ,
                   "Contemporary" = "Contemporary",
                   "Classics" = "Classics",
                   "Childrens" = "Childrens",
                   "Adventure" = "Adventure",
                   "Adult" = "Adult",
                   "All" = "*") #tdeal with the last category later

dashboardPage(
  dashboardHeader(
    title = "Goodreads Statistics"
  ),
  dashboardSidebar(
    sliderInput("year", "Year:",
                min = 1900, max = 2022, value = c( 2000,2022)),
    radioButtons("genre", "Genre:",
                 genres_vector)
  ),
  
  dashboardBody(
   # mainPanel(
      box("Related genres", 
        plotlyOutput("pieChartPlotly", height = 400, width = 500)
      ),
      box("Book genres across languages", 
        plotlyOutput("barChartPlotly", height = 400, width = 500)
      ),
      fluidRow(
      shinydashboard::infoBoxOutput("yearInfoBox"),
      shinydashboard::infoBoxOutput("genreInfoBox"),
      shinydashboard::infoBoxOutput("reviewInfoBox")
    ),
    fluidRow(
      shinydashboard::valueBoxOutput("priceValueBox"),
      shinydashboard::valueBoxOutput("ratingValueBox"),
      shinydashboard::valueBoxOutput("reviewValueBox")
    ),
    tabBox(width=12,
        tabPanel("Table", 
    
      dataTableOutput("mainDataTable")
    ),
      tabPanel("Plot Price-rating", 
        
          plotlyOutput("priceRatingPlotly")
        ),
    tabPanel("Plot Reviews-rating", 
             
             plotlyOutput("reviewRatingPlotly")
    )
      )
    )
  #)
)
