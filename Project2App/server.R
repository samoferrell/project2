library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(jsonlite)
library(tidyr)
shinyServer(function(input, output, session) {
  endpoint <- eventReactive(input$submit, {input$endpoint})
  show_name <- eventReactive(input$submit, {input$show_name})

output$summary <- DT::renderDataTable({


  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed) |> 
    select(!summary)
  subsetted <- subset(all_shows, name == show_name())
  id <- subsetted$id
  
  if (input$RB == "all"){
    return(all_shows)
  }
  
  if (endpoint() == "general") {
     return(subsetted)} 
  else {
    new_url <- paste0(url, "/", id, "/", endpoint())}
  
  id_info_specific <- httr::GET(new_url)
  parsed_specific <- fromJSON(rawToChar(id_info_specific$content))
  info_specific <- tibble::as_tibble(parsed_specific)
      
      # If character we will expand the columns
      if (endpoint() == "cast"){
      cast_info <- info_specific |>
        unnest_wider(col = person, names_sep = "_") |>
        unnest_wider(col = character, names_sep = "_")
      return(cast_info)
      }
      
  else {return(info_specific)}
})
})
