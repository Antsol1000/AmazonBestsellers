#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

shinyServer(function(input, output) {

  output$mainDataTable <- renderDataTable({
    datatable(
      data %>%
        filter(Year == input$year) %>%
        select(-Year)
    )
  })

  output$priceRatingPlotly <- renderPlotly({
    p <- data %>%
      filter(Year == input$year) %>%
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

  output$priceValueBox <- renderValueBox({
    s <- 0
    r <- input$mainDataTable_rows_selected
    if (!is.null(r)) {
      d <- data %>%
        filter(Year == input$year)
      s <- d[r,] %>%
        select(Price) %>%
        sum(.)
    }
    valueBox(
      value = tags$p(paste('TOTAL: $', toString(s)),
                     style = "font-size: 250%;")
    )
  })

  output$reviewGauge <- renderGauge({
    thisReviews <- reviews %>%
      filter(Year == input$year) %>%
      select(Reviews) %>%
      max(.)

    gauge(thisReviews, min = 0, max = maxReviews,
          gaugeSectors(
            success = c(0.7 * maxReviews, maxReviews),
            warning = c(0.3 * maxReviews, 0.7 * maxReviews),
            danger = c(0, 0.3 * maxReviews)
          )
    )
  })

})

data <- read.csv("bestsellers.csv", sep = ",")

reviews <- data %>%
  aggregate(Reviews ~ Year, ., sum)

maxReviews <- reviews %>%
  select(Reviews) %>%
  max(.)
