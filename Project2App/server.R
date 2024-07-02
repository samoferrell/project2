library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(jsonlite)
library(tidyr)
library(purrr)

# Data Download 
shinyServer(function(input, output, session) {
  endpoint <- eventReactive(input$submit, {input$endpoint})
  show_name <- eventReactive(input$submit, {input$show_name})
  RB <- eventReactive(input$submit, {input$RB})
  min_show <- eventReactive(input$submit, {input$min_show})
  min_ep <- eventReactive(input$submit, {input$min_ep})
  

 reactive_data <- eventReactive(input$submit, {

  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)  |> 
    unnest_wider(network, names_sep = "_") |>
    unnest_wider(network_country, names_sep = "_") |>
    unnest_wider(rating, names_sep = "_") |>
    select(id,name,type,language,genres, rating_average, status,runtime,premiered,
           ended, network_name, network_country_name) |>
    filter(rating_average >= min_show())
    
  
  subsetted <- subset(all_shows, name == show_name())
  id <- subsetted$id
  
  if (RB() == "all"){
    output$columns <- renderUI({
    checkboxGroupInput("CB", "Select Columns to Keep for Download", choices = colnames(all_shows)) })
    return(all_shows)
  }
  
  if (endpoint() == "general") {
    output$columns <- renderUI({
      checkboxGroupInput("CB", "Select Columns to Keep for Download", choices = colnames(subsetted)) })
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
        unnest_wider(col = character, names_sep = "_") |>
        select(person_name,person_birthday,person_deathday,person_gender,character_name,self)
        output$columns <- renderUI({
        checkboxGroupInput("CB", "Select Columns to Keep for Download", choices = colnames(cast_info)) })
        cast_info <- cast_info |> 
          filter(
            if (input$gender == "Both") {
              person_gender %in% c("Male", "Female")
            } else if (input$gender == "Female") {
              person_gender == "Female"
            } else {
              person_gender == "Male"
            }
          )
        return(cast_info)
      }
      if (endpoint() == "seasons"){
        season_info <- info_specific |>
          select(number, episodeOrder, premiereDate, endDate)
        output$columns <- renderUI({
        checkboxGroupInput("CB", "Select Columns to Keep for Download", choices = colnames(season_info)) })
        return(season_info)
        
      }
      if (endpoint() == "episodes"){
        episode_info <- info_specific |>
          unnest_wider(rating, names_sep = "_") |>
          select(name, season, number, type, airdate, runtime, rating_average)
        output$columns <- renderUI({
          checkboxGroupInput("CB", "Select Columns to Keep for Download", choices = colnames(episode_info)) })
        episode_info <- episode_info |>
          filter(rating_average >= min_ep())
        return(episode_info)}
})
 
 output$summary <- DT::renderDataTable({
   reactive_data() 
 })
 
output$downloadData <- downloadHandler(
  filename = function() {
    paste('show-data-','-',Sys.Date(),'.csv',sep='')
  },
  content = function(file){
    write.csv(reactive_data() |> select(input$CB), 
              file, row.names = FALSE)
  }

)
})
