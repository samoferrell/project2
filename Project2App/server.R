library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(jsonlite)
library(tidyr)
shinyServer(function(input, output, session) {
  endpoint <- eventReactive(input$submit, {input$endpoint})
  show_name <- eventReactive(input$submit, {input$show_name})
  RB <- eventReactive(input$submit, {input$RB})
output$summary <- DT::renderDataTable({

  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)  |> 
    select(!c(summary,url,id))
  subsetted <- subset(all_shows, name == show_name())
  id <- subsetted$id
  
  if (RB() == "all"){
    output$columns <- renderUI({
    checkboxGroupInput("CB", "Select Columns to Keep", choices = colnames(all_shows)) })
    return(all_shows)
  }
  
  if (endpoint() == "general") {
    output$columns <- renderUI({
      checkboxGroupInput("CB", "Select Columns to Keep", choices = colnames(subsetted)) })
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
        output$columns <- renderUI({
        checkboxGroupInput("CB", "Select Columns to Keep", choices = colnames(cast_info)) })
        return(cast_info)
      }
      
  else {
    output$columns <- renderUI({
      checkboxGroupInput("CB", "Select Columns to Keep", choices = colnames(info_specific)) })
    return(info_specific)}
  
})
})
