library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(jsonlite)
library(tidyr)
shinyServer(function(input, output, session) {
  
output$summary <- DT::renderDataTable({
  endpoint <- input$endpoint
  show_name <- input$show_name
  
  observeEvent(input$submit, {
    updateTextInput(session, "show_name", value = input$show_name) 
    updateSelectInput(session, "endpoint", selected = input$endpoint)})
  
  url = "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)
      subsetted <- subset(all_shows, name == show_name)
      id <- subsetted$id
      new_url <- paste0(url, "/", id, "/", endpoint)
      id_info_specific <- httr::GET(new_url)
      parsed_specific <- fromJSON(rawToChar(id_info_specific$content))
      info_specific <- tibble::as_tibble(parsed_specific)
      # If character we will expand the columns
      info_specific <- info_specific |>
        unnest_wider(col = person, names_sep = "_") |>
        unnest_wider(col = character, names_sep = "_")
      info_specific
})
})
