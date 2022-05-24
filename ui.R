#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(flexdashboard)

# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(
    title = "Amazon Bestsellers"
  ),
  dashboardSidebar(
    sliderInput("year", "Year:", min = 2009, max = 2022, value = 2022),
    gaugeOutput("reviewGauge"),
    valueBoxOutput("priceValueBox")
  ),
  dashboardBody(
    dataTableOutput("mainDataTable"),
    plotlyOutput("priceRatingPlotly"),
  )
)
