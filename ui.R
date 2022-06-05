library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(magrittr)
library(billboarder)
library(reshape2)

genres_vector <- c("All" = "*",
                   "Adult" = "Adult",
                   "Adventure" = "Adventure",
                   "Childrens" = "Childrens",
                   "Classics" = "Classics",
                   "Classics" = "Classics",
                   "Contemporary" = "Contemporary",
                   "Fantasy" = "Fantasy",
                   "Fiction" = "Fiction",
                   "Historical" = "Historical",
                   "Historical Fiction" = "Historical Fiction",
                   "Literature" = "Literature",
                   "Mystery" = "Mystery",
                   "Nonfiction" = "Nonfiction",
                   "Novels" = "Novels",
                   "Paranormal" = "Paranormal",
                   "Romance" = "Romance",
                   "Science Fiction" = "Science Fiction",
                   "Young Adult" = "Young Adult")

dashboardPage(
  dashboardHeader(
    title = "Goodreads Statistics"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Preview", tabName = "preview"),
      menuItem("Details", tabName = "details")
    ),
    sliderInput("year", "Year:",
                min = 1950, max = 2022, value = c(2000, 2022), ticks = FALSE),
    radioButtons("genre", "Genre:",
                 genres_vector)
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "preview",
        fluidRow(
          box("Related genres", plotlyOutput("pieChartPlotly", height = 400, width = 500)),
          box("Book genres across languages", plotlyOutput("barChartPlotly", height = 400, width = 500))
        ),
        fluidRow(
          box("Average rating depending on likes", status = "primary", width = 6,
              sliderInput("likes", "Likes", min = 1, max = 100, value = c(10, 90), post = " %"),
              plotlyOutput("Dplotavgrating")),
          box("Page count and rating relation", status = "primary", width = 6,
              sliderInput("rating", "Rating", min = 0.0, max = 5.0, value = c(0.25, 4.75), step = 0.1),
              plotlyOutput("DPlotpages")
          )
        )
      ),
      tabItem(
        tabName = "details",
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
          tabBox(
            width = 12,
            tabPanel("Table", dataTableOutput("mainDataTable")),
            tabPanel("Plot Price-rating", plotlyOutput("priceRatingPlotly")),
            tabPanel("Plot Reviews-rating", plotlyOutput("reviewRatingPlotly"))
          )
        )
      )
    )
  )

)

