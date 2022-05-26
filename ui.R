library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(magrittr)

dashboardPage(
  dashboardHeader(
    title = "Amazon Bestsellers"
  ),
  dashboardSidebar(
    sliderInput("year", "Year:",
                min = 2009, max = 2022, value = 2022),
    radioButtons("genre", "Genre:",
                 c("All" = "*", "Fiction" = "^Fiction", "Non Fiction" = "^Non"))
  ),
  dashboardBody(
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
    fluidRow(
      dataTableOutput("mainDataTable")
    ),
    fluidRow(
      plotlyOutput("priceRatingPlotly")
    )
  )
)
