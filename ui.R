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
    title = "Goodreads Statistics", titleWidth = 450
  ),
  
  dashboardSidebar(
    img(src="logopp.png",width="100%") ,
    sidebarMenu(
      menuItem("Preview", tabName = "preview"),
      menuItem("Details", tabName = "details"),
      menuItem("Help", tabName = "help")
    ),
    sliderInput("year", "Year:",
                min = 1950, max = 2022, value = c(2000, 2022), ticks = FALSE),
    radioButtons("genre", "Genre:",
                 genres_vector)
  ),

  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    tags$head(tags$link(rel="shortcut icon", href="https://bip.put.poznan.pl/sites/default/files/logo_0_0.png")),
    tabItems(
      tabItem(
        tabName = "preview",
        fluidRow(
          box("Related genres", status = "primary",plotlyOutput("pieChartPlotly")),
          box("Book genres across languages", plotlyOutput("barChartPlotly"))
        ),
        fluidRow(
          box("Average rating depending on likes", status = "primary", width = 6,
              sliderInput("likes", "Likes", min = 1, max = 100, value = c(10, 90), post = " %"),
              plotlyOutput("Dplotavgrating")),
          box("Page count and rating relation", status = "primary", width = 6,
              sliderInput("rating", "Rating", min = 0.0, max = 5.0, value = c(4.0, 4.5), step = 0.1),
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
            tabPanel("Plot price-rating", plotlyOutput("priceRatingPlotly")),
            tabPanel("Plot reviews-rating", plotlyOutput("reviewRatingPlotly"))
          )
        )
        
      ),
      tabItem(
        tabName = "help",
          mainPanel(
            p(),
            h1("Help"),
            h2("About"),
            p("The goal of this dashboard is to convey ground - level knowledge about books by means of using the data gathered from useres of the Goodreads platform.",
               "In the ", strong("Preview"), " tab one can find high level information about the books - distrubution of genres across languages, how likes relate to ratings etc.",
              "For a closer examination of particular book instances visit", strong("Details"), "."),
            p("In the side menu use the slider to pick the time period you are interested in, and the genre you'd like to examine further."),
            
            h2("Preview"),
            h3("Related genres"),
            p("On this chart you can take a look which genres are related to the genre picked in the side menu in the period listed. You can remove genres listed in the pie chart by clicking on the selected one on the list located on the right side of the plot."),
            
            h3("Book genres across languages"),
            p("On this chart you can take a look what is the distribution of genres across a pre-picked subset of countries. Hover your mouse over the selected color and the name of the genre will appear. You can also choose ", 
              strong("Compare data on hover"), " ( right upper corner) for a more comprehensive exapansion of the genres.", " In order to make it visually distinct from the orther boxes, the color accent is grey instead of blue, to indicate it being independed of the selected genre from the side menu."),
            h3("Average rating depending on likes"),
            p("Choose the subset of books you'd like to examine (based on % of likes) and see how the average rating of the books is related to the likes."),
            
            h3("Page count and rating relation"),
            p("Examine wheather the average rating of a book relates somehow to it's length. Adjust the slider to set the average rating of the book, and see wtheter there are some trends in the book length.",
              "The blue opaque histogram is not dynamic - it gives us an idea of the changes, it prepresents input unfiltered by the slider."),
            
            
            h2("Details"),
            p("In this section take a closer look at particuar books, using the table you can select instances which will be the highlighted on the plots related (in the tabs).",
              "From the top the three widgets are activated when books are picked, you can pick books by filtering the table, the lower bound of the price will be displayed on the ", 
              strong("price"), " widget, average rating and the average number of reviews will be displayed in an analogous way by the orther widgets.", " Although there are missing values, as prior mentioned, the lower bound is displayed, with a leading \">\" sign."),
            p(" In the tabs you can find scatter plots relating price and ratings, as well as reviews and ratings.")
          
            
          )

        )
        
      )
    )
  )

