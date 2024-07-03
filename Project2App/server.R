library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(jsonlite)
library(tidyr)
library(purrr)

shinyServer(function(input, output, session) {
# all my reactive events are below:
  endpoint <- eventReactive(input$submit, {input$endpoint})
  show_name <- eventReactive(input$submit, {input$show_name})
  RB <- eventReactive(input$submit, {input$RB})
  min_show <- eventReactive(input$submit, {input$min_show})
  min_ep <- eventReactive(input$submit, {input$min_ep})
  
# I will create my data to generate when the submit button is hit. 
# This will also allow the data to be downloaded because I can call
# the reactive data easily
  
# for all the endpoints, we create checkboxes based on the current columns 
# for the user to specify and keep
  
 reactive_data <- eventReactive(input$submit, {

# first I will start with a url and access the main show data
  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)  |> 
# a lot of the columns are saved as a list / data frame so I will expand them:
    unnest_wider(network, names_sep = "_") |>
    unnest_wider(network_country, names_sep = "_") |>
    unnest_wider(rating, names_sep = "_") |>
# now I will select only the columns that are of interest.
    select(id,name,type,language,genres, rating_average, status,runtime,premiered,
           ended, network_name, network_country_name) 
# I will now allow the user to filter by adjusting a slider and hitting "GO!"

# to find general information about a single show, I will subset the show data by matching the name and then ID  
  subsetted <- subset(all_shows, name == show_name())
  id <- subsetted$id

# to display all the general show data, the radio button must be selected as all and GO! must be clicked.  
  if (RB() == "all"){
    output$columns <- renderUI({
    checkboxGroupInput("CB", "Select Columns to Keep for Download", 
                       choices = colnames(all_shows),
                       selected = colnames(all_shows))})
    shows_rated <- all_shows |>
      filter(rating_average >= min_show())
    return(shows_rated)
  }
# when the endpoint is specified to general, we will not update the url and instead just return 
# the subsetted data.
  if (endpoint() == "general") {
    output$columns <- renderUI({
      checkboxGroupInput("CB", "Select Columns to Keep for Download", 
                         choices = colnames(subsetted),
                         selected = colnames(subsetted)) })
     return(subsetted)} 
# for all other endpoints specified, we will update the url  
  else {
    new_url <- paste0(url, "/", id, "/", endpoint())}
  id_info_specific <- httr::GET(new_url)
  parsed_specific <- fromJSON(rawToChar(id_info_specific$content))
  info_specific <- tibble::as_tibble(parsed_specific)

# for our cast endpoint, we will expand the data frame and select only the specified columns        
      if (endpoint() == "cast"){
      cast_info <- info_specific |>
        unnest_wider(col = person, names_sep = "_") |>
        unnest_wider(col = character, names_sep = "_") |>
        select(person_name,person_birthday,person_deathday,person_gender,character_name,self)
        output$columns <- renderUI({
        checkboxGroupInput("CB", "Select Columns to Keep for Download", 
                           choices = colnames(cast_info),
                           selected = colnames(cast_info)) })
        cast_info <- cast_info |> 
# we will also use the inputted gender specification to filter the data set
          filter(
            if (input$gender == "Both") {
              person_gender %in% c("Male", "Female")} 
            else if (input$gender == "Female") {
              person_gender == "Female"} 
            else {
                person_gender == "Male"})
        return(cast_info)}
  
# for our seasons endpoint, we will select our specified columns. 
      if (endpoint() == "seasons"){
        season_info <- info_specific |>
          select(number, episodeOrder, premiereDate, endDate)
        output$columns <- renderUI({
        checkboxGroupInput("CB", "Select Columns to Keep for Download", 
                           choices = colnames(season_info),
                           selected = colnames(season_info)) })
        return(season_info)}
  
# for our episodes endpoint, we will apply an input filter to allow the user to apply a minimum episode
# rating value 

      if (endpoint() == "episodes"){
        episode_info <- info_specific |>
          unnest_wider(rating, names_sep = "_") |>
          select(name, season, number, type, airdate, runtime, rating_average)
        output$columns <- renderUI({
          checkboxGroupInput("CB", "Select Columns to Keep for Download", 
                             choices = colnames(episode_info),
                             selected = colnames(episode_info))})
        episode_info <- episode_info |>
          filter(rating_average >= min_ep())
        return(episode_info)}
  })
 
 output$summary <- DT::renderDataTable({
   reactive_data() |> select(input$CB)
 }) 
 
 # this is how we will download the data
output$downloadData <- downloadHandler(
  filename = function() {
    paste('show-data-','-',Sys.Date(),'.csv',sep='')
  },
  # this will ensure the user downloads the data that has been subsetted correclty.
  content = function(file){
    write.csv(reactive_data() |> select(input$CB), 
              file, row.names = FALSE)
  })
})
