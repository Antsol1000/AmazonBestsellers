library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(magrittr)
library(tidyverse)
library(billboarder)
library(reshape2)


#data <- read.csv("data/bestsellers.csv", sep = ",")
library(tidyverse)
library(dplyr)

languagesOfInterest <- c("English", "German", "Russian","Chinese", "Czech", "Polish", "French", "Spanish", "Arabic", "Turkish","Japanese")

df <- read.csv("data/Best_Books_Ever.csv", sep = ",")
colnames <- colnames(df)
columns_drop <- c("characters", "bookFormat", "edition", "coverImg", "bookId", "setting", "bbeVotes", "ratingsByStars", "isbn")
#df$genres = substr(df$genres,2,nchar(df$genres)-1)
data <- df %>%  .[,setdiff(names(.),columns_drop)] %>% separate_rows(genres, sep = ",")
data$genres <- trimws(data$genres, which = c("both"), whitespace = "([\\[\\]\t\r\n']| ')")
data$Year <- format(as.Date(data$firstPublishDate, format="%m/%d/%Y"),"%Y")
data$publishDate <- format(as.Date(data$publishDate, format="%m/%d/%Y"),"%Y")
data<- data %>% mutate(Year = coalesce(Year,publishDate)) %>% select(., -c(publishDate, firstPublishDate, description, awards))
data$genre <- data$genres
data <- data %>% .[,setdiff(names(.),c("genres"))]


reviews <- data %>% distinct(., title, .keep_all = TRUE) %>%
  aggregate(numRatings ~ Year, ., sum)

maxReviews <- reviews %>%
  select(numRatings) %>%
  max(.)

getReviewsForYear <- function(year) {
  reviews %>%
    filter(Year >= year[1]) %>%
    filter(Year <= year[2]) %>%
    select(numRatings) %>%
    max(.)
}

getDataByYearAndAllGenres <- function(year) {
  out <-data %>%
    filter(Year >= year[1]) %>%
    filter(Year <= year[2]) %>%
  group_by(genre) %>%
    summarise(count = n(), totalRev = sum(numRatings)) %>% arrange(desc(count))
  total_sum <- sum(out$count)
  out<- out %>% mutate( genre = ifelse(count < total_sum[1] *0.015, "Other", genre))
  out
    
}

getDataByYearAndGenre <- function(year, selected_genre) {
  if(selected_genre == "*"){
    out <- data %>%
      filter(Year >= year[1]) %>%
      filter(Year <= year[2]) %>% distinct(., title, .keep_all = TRUE)
    out$genre = ""
    
  }else{
    out <- data %>%
      filter(Year >= year[1]) %>%
      filter(Year <= year[2]) %>%
     filter(genre %in% selected_genre)
}
  out
}

getDataByYearAndSubGenre <- function(year, selected_genre) {
  selected_books <-getDataByYearAndGenre(year, selected_genre) %>% select(., title)
  out <- data %>% filter(., genre != selected_genre) %>% dplyr::inner_join(., selected_books, by = "title") %>%
    group_by(genre) %>%
    summarise(count = n(), totalRev = sum(numRatings)) %>% arrange(desc(count))
  total_sum <- sum(out$count)
  out<- out %>% mutate( genre = ifelse(count < total_sum[1] *0.015, "Other", genre))
  out
}

getDataByYearAndGenres <- function(year, selected_genre) {
  out <- data %>% filter(Year >= year[1]) %>%
    filter(Year <= year[2]) %>% filter(genre != "") %>% filter(language %in% languagesOfInterest) %>% 
    group_by(language, genre) %>%
    summarise(count = n(), totalRev = sum(numRatings)) %>% arrange(desc(language)) %>% filter(language != "")
  total_sumsByCountries <- out %>% group_by(language) %>% summarise(countAll = sum(count)) %>% select(., language, countAll) %>% filter(., countAll > 10) %>% dplyr::inner_join(., out, by = "language")
  out<- total_sumsByCountries %>% mutate(genre = ifelse(count <  countAll *0.02, "Other", genre)) %>%  group_by(language, genre) %>%
    summarise(count = sum(count), countAll = mean(countAll)) %>% arrange(desc(language))
  out$percent = out$count*100/out$countAll
  out
  
}

shinyServer(function(input, output) {

  output$yearInfoBox <- renderInfoBox({

    shinydashboard::infoBox("Year",paste(input$year[1], "-", input$year[2]),
                            icon = icon("calendar", lib = "font-awesome"),
                            color = "purple")
  })

  output$genreInfoBox <- renderInfoBox({
    text <- ifelse(input$genre == "*", "All genres",
                   input$genre)
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
    displayStr <-  ""
    r <- input$mainDataTable_rows_selected
    if (!is.null(r)) {
      d <- getDataByYearAndGenre(input$year, input$genre)
      tempS <- d[r,] %>% filter(., price != "")
      s <- tempS %>%
        select(price) %>% transform(., price = as.numeric(price)) %>%
        sum(.)
      if(nrow(tempS) < nrow(d[r,])){displayStr <- ">"}
    }

    shinydashboard::valueBox(paste(displayStr, s, "$"), "Total price",
                             icon = icon("money-bill", lib = "font-awesome"),
                             color = ifelse(s > 100, "red", "purple"))
  })

  output$ratingValueBox <- shinydashboard::renderValueBox({
    s <- 0
    r <- input$mainDataTable_rows_selected
    if (!is.null(r)) {
      d <- getDataByYearAndGenre(input$year, input$genre)
      s <- d[r,] %>%
        select(rating) %>%
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
        select(numRatings) %>%
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
      getDataByYearAndGenre(input$year, input$genre)
    )
  })

  output$priceRatingPlotly <- renderPlotly({
    p <- getDataByYearAndGenre(input$year, input$genre) %>%
      mutate(Selected = FALSE)

    for (i in input$mainDataTable_rows_selected) {
      p[i,]$Selected <- TRUE
    }

    p$price <- as.numeric(p$price)
    p <- p %>% filter(., price != "") %>%
      ggplot(aes(price, rating, label = title, fill = Selected)) +
      geom_point() +
      #scale_y_continuous(breaks = round(seq(0, max(p$price), by = 0.5), 1)) +
      theme_minimal() +
      scale_fill_manual(values = c("blue4", "maroon3"))
    
    ggplotly(p)
  })
  
  output$reviewRatingPlotly <- renderPlotly({
    p <- getDataByYearAndGenre(input$year, input$genre) %>%
      mutate(Selected = FALSE)
    
    for (i in input$mainDataTable_rows_selected) {
      p[i,]$Selected <- TRUE
    }
    
    p <- p %>% filter(., rating != "") %>% filter(., numRatings != "") %>%
      ggplot(aes(numRatings, rating, label = title, fill = Selected)) +
      geom_point() + 
      #scale_y_continuous(breaks = round(seq(0, max(p$price), by = 0.5), 1)) +
      theme_minimal() +
      scale_fill_manual(values = c("blue4", "maroon3"))
    
    ggplotly(p)
  })
  
  output$pieChartPlotly <- renderPlotly({
    if(input$genre == "*"){
      p<- getDataByYearAndAllGenres(input$year)
    }else {
      p <- getDataByYearAndSubGenre(input$year, input$genre)
    }
    
    fig <- plot_ly(p, labels = ~genre, values = ~count, type = 'pie')
    
    fig <- fig %>% layout(
                          
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    fig
  })
  
  output$barChartPlotly <- renderPlotly({
    p <- getDataByYearAndGenres(input$year, input$genre)
    y <- p %>% plot_ly(x = ~language, y = ~percent, type = 'bar', 
                 name = ~genre, color = ~genre) %>% 
      layout(yaxis = list(title = 'Count'), barmode = 'stack') %>% layout(showlegend = FALSE,  xaxis = list(title = ''), yaxis = list(title = 'Distribution in %'))
    y
  })
  
  output$Dplotavgrating <- renderPlotly({
    p <- getDataByYearAndGenre(input$year, input$genre) %>%
      filter(likedPercent >= input$likes[1]) %>%
      filter(likedPercent <= input$likes[2]) 
    fit <- density(p$rating)
    plot_ly(x = p$rating, type = "histogram", name = "Histogram" ,xbins = list(size = 0.25)) %>%
      layout(
        xaxis = list(
          range=c(0,5)
        ), yaxis= list(showgrid = FALSE)) %>%
      add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$Dplotavgrating <- renderPlotly({
    p <- getDataByYearAndGenre(input$year, input$genre) %>%
      filter(likedPercent >= input$likes[1]) %>%
      filter(likedPercent <= input$likes[2]) 
    fit <- density(p$rating)
    plot_ly(x = p$rating, type = "histogram", name = "Histogram" ,xbins = list(size = 0.25)) %>%
      layout(
        xaxis = list(
          range=c(0,5), title = "average rating"
        ), yaxis= list(showgrid = FALSE)) %>%
      add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$DPlotpages <- renderPlotly({
    temp <- getDataByYearAndGenre(input$year, input$genre) %>%
      filter(pages != "") %>%
      filter(!is.na(pages)) %>%
      mutate(pages = as.numeric(pages))
    p <-temp %>%
      filter(rating >= input$rating[1]) %>%
      filter(rating <= input$rating[2])
    plot_ly(x = temp$pages, type = "histogram" ,xbins = list(size = 25), opacity=0.5, name = 'all books') %>%
      layout(xaxis = list(
        range=c(0,1000), title = "page count"
      ), 
         yaxis= list(showgrid = FALSE)) %>%
      add_histogram(x = p$pages, opacity=1, name = 'selected') %>% 
      layout(barmode = "overlay")
  })

})
