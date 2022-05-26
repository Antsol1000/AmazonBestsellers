library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(magrittr)


data <- read.csv("data/bestsellers.csv", sep = ",")

reviews <- data %>%
  aggregate(Reviews ~ Year, ., sum)

maxReviews <- reviews %>%
  select(Reviews) %>%
  max(.)

getReviewsForYear <- function(year) {
  reviews %>%
    filter(Year == year) %>%
    select(Reviews) %>%
    max(.)
}

getDataByYearAndGenre <- function(year, genre) {
  data %>%
    filter(Year == year) %>%
    filter(grepl(genre, Genre))
}


shinyServer(function(input, output) {

  output$yearInfoBox <- renderInfoBox({
    shinydashboard::infoBox("Year", input$year,
                            icon = icon("calendar", lib = "font-awesome"),
                            color = "purple")
  })

  output$genreInfoBox <- renderInfoBox({
    text <- ifelse(input$genre == "*", "Fiction & Non fiction",
                   ifelse(input$genre == "^Non", "Non fiction", "Fiction"))
    shinydashboard::infoBox("Genre", text,
                            icon = icon("book", lib = "font-awesome"),
                            color = "yellow")
  })

  output$reviewInfoBox <- renderInfoBox({
    thisReviews <- round(getReviewsForYear(input$year) / 1000, 1)

    shinydashboard::infoBox("Reviews in year", paste(thisReviews, "K"),
                            icon = icon("pen", lib = "font-awesome"),
                            color = "aqua")
  })

  output$priceValueBox <- shinydashboard::renderValueBox({
    s <- 0
    r <- input$mainDataTable_rows_selected
    if (!is.null(r)) {
      d <- getDataByYearAndGenre(input$year, input$genre)
      s <- d[r,] %>%
        select(Price) %>%
        sum(.)
    }

    shinydashboard::valueBox(paste(s, "$"), "Total price",
                             icon = icon("money-bill", lib = "font-awesome"),
                             color = ifelse(s > 100, "red", "purple"))
  })

  output$ratingValueBox <- shinydashboard::renderValueBox({
    s <- 0
    r <- input$mainDataTable_rows_selected
    if (!is.null(r)) {
      d <- getDataByYearAndGenre(input$year, input$genre)
      s <- d[r,] %>%
        select(User.Rating) %>%
        as.matrix() %>%
        mean() %>%
        round(digits = 2)
    }

    shinydashboard::valueBox(s, "Average user rating",
                             icon = icon("thumbs-up", lib = "font-awesome"),
                             color = ifelse(s > 4.7, "green", "yellow"))
  })

  output$reviewValueBox <- shinydashboard::renderValueBox({
    thisReviews <- getReviewsForYear(input$year)
    s <- 0
    r <- input$mainDataTable_rows_selected
    if (!is.null(r)) {
      d <- getDataByYearAndGenre(input$year, input$genre)
      s <- d[r,] %>%
        select(Reviews) %>%
        as.matrix() %>%
        mean() %>%
        divide_by(1000) %>%
        round(digits = 1)
    }

    shinydashboard::valueBox(paste(s, "K"), "Average reviews number",
                             icon = icon("pen", lib = "font-awesome"),
                             color = ifelse(s > .7 * thisReviews / 50000, "green", "aqua"))
  })

  output$mainDataTable <- renderDataTable({
    datatable(
      getDataByYearAndGenre(input$year, input$genre) %>%
        select(-Year)
    )
  })

  output$priceRatingPlotly <- renderPlotly({
    p <- getDataByYearAndGenre(input$year, input$genre) %>%
      mutate(Selected = FALSE)

    for (i in input$mainDataTable_rows_selected) {
      p[i,]$Selected <- TRUE
    }

    p <- p %>%
      ggplot(aes(Price, User.Rating, label = Name, fill = Selected, size = 1.5)) +
      geom_point() +
      theme_minimal() +
      scale_fill_manual(values = c("blue4", "maroon3"))

    ggplotly(p)
  })

})
