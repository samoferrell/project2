library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(jsonlite)
library(tidyr)
library(purrr)
library(see)
library(ggplot2)

shinyServer(function(input, output, session) {

  
  
  
  
  
  # all my reactive events are below:
  endpoint <- eventReactive(input$submit, {input$endpoint})
  show_name <- eventReactive(input$submit, {input$show_name})
  RB <- eventReactive(input$submit, {input$RB})
  min_show <- eventReactive(input$submit, {input$min_show})
  min_ep <- eventReactive(input$submit, {input$min_ep})
  #selected_genres <- reactive(input$selected_genres)
  
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

# TAB 3
###################################

###### Numerical Show Summaries #########
output$genre_data <- DT::renderDataTable({
url <- "https://api.tvmaze.com/shows"
id_info <- httr::GET(url)
parsed <- fromJSON(rawToChar(id_info$content))
all_shows <- tibble::as_tibble(parsed)
filtered <- all_shows |>
 unnest_wider(rating, names_sep = "_") |>
  select(name,genres,rating_average) |>
  filter(map_lgl(genres, ~ all(input$selected_genres %in% .x)))
filtered
})
output$summary_table <- renderPrint({
  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)
  filtered <- all_shows |>
    unnest_wider(rating, names_sep = "_") |>
    select(name,genres,rating_average) |>
    filter(map_lgl(genres, ~ all(input$selected_genres %in% .x)))
summary_table <- round(match.fun(input$stats)(filtered$rating_average),2)

if (input$stats %in% c("mean", "median")){
  print(c(input$stats,summary_table))
}
else if (input$stats == "max"){print(c(input$stats, 
                                       summary_table, 
                                       filtered$name[which.max(filtered$rating_average)]))}
else if (input$stats == "min"){print(c(input$stats,
                                       summary_table,
                                       filtered$name[which.min(filtered$rating_average)]))}

})  
###### Contingency Tables #########
output$tables <- renderPrint({
  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)
  data <- all_shows |>
    unnest_wider(rating, names_sep = "_") |>
    unnest_wider(schedule, names_sep = "_") |>
    select(name,type,language, status, runtime,genres,rating_average, schedule_days) |>
    mutate(rating = case_when(
      rating_average >= 0 & rating_average <= 2.5 ~ "Bad",
      rating_average > 2.5 & rating_average <= 5 ~ "Poor",
      rating_average > 5 & rating_average <= 7.5 ~ "Good",
      rating_average > 7.5 & rating_average <= 10 ~ "Great"))
  if (length(input$table_vars) == 2) {
    table <- table(data[[input$table_vars[1]]], 
                   data[[input$table_vars[2]]])
    print(table)
  } 
  else if (length(input$table_vars) == 3) {
    table <- table(data[[input$table_vars[1]]], 
                   data[[input$table_vars[2]]], 
                   data[[input$table_vars[3]]])
    print(table)
  }
  else {
    print("Select 2 to 3 variables")}
})

###### Plots #########
output$show_rating_plot <- renderPlot({
  
 # average show rating based on premiered year - grouped and colored by type
  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)
  data <- all_shows |>
    unnest_wider(rating, names_sep = "_") |>
    mutate(year = substr(premiered,1,4))
  
  ggplot(data, aes(x = year, y = rating_average, color = type)) +
  geom_point(size = 3) +
    theme_minimal() +
    scale_color_manual(values = c("green","blue","pink","black","red"))
})
  output$ep_rating_plot <- renderPlot({
    
  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)
  
  # I will now allow the user to filter by adjusting a slider and hitting "GO!"
  
  # to find general information about a single show, I will subset the show data by matching the name and then ID  
  subsetted <- subset(all_shows, name == input$show_eps)
  id <- subsetted$id 
  new_url <- paste0(url, "/", id, "/", "episodes")
  season_url <- paste0(url, "/", id, "/", "seasons")
  season_info <-httr::GET(season_url)
  season_parsed <- fromJSON(rawToChar(season_info$content))
  all_season <- tibble::as_tibble(season_parsed)
  season_data <- all_season |>
    mutate(season = as.factor(number))
  
  ep_info <- httr::GET(new_url)
  ep_parsed <- fromJSON(rawToChar(ep_info$content))
  all_eps <- tibble::as_tibble(ep_parsed)
  episode_ratings <- all_eps |>
    unnest_wider(rating, names_sep = "_") |>
    mutate(season = as.factor(season))

  seasons <- left_join(season_data, episode_ratings |>
                         group_by(season) |>                       
                         summarise(mean_rating = mean(rating_average, na.rm = TRUE)),
                       by = "season")
  
  if (input$season_avg == "no"){
    
  g <- ggplot(episode_ratings, aes(x = season, y = rating_average, 
                              group = season,
                              fill = season)) +
    geom_violindot(dots_size = 0.5, binwidth = 0.25,fill_dots = "black") +
    scale_fill_material_d()+
    xlab ("Season Number") +
    ylab ("Average Episode Rating") +
    ylim(0,10) +
    labs(title = "Episode Rating Distribution by Season")
  }
  else if (input$season_avg == "yes"){
    g <- ggplot(seasons, aes(x = season, y = mean_rating, fill = season)) +
      geom_bar(stat = "identity") +
      ylim(0,10) +
      labs(
        x = "Season",
        y = "Episode Average",
        title = "Episode Average across Seasons")
    
    
  }
  if (input$facet == "no"){
  g
    }
  else if (input$facet == "yes"){
    g + 
      facet_wrap(~ season)
  }
})

output$cast_plot <- renderPlot({
  url <- "https://api.tvmaze.com/shows"
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)
  
  # I will now allow the user to filter by adjusting a slider and hitting "GO!"
  
  # to find general information about a single show, I will subset the show data by matching the name and then ID  
  subsetted <- subset(all_shows, name == input$show_eps)
  id <- subsetted$id 
  new_url <- paste0(url, "/", id, "/", "cast")
  cast_info <- httr::GET(new_url)
  cast_parsed <- fromJSON(rawToChar(cast_info$content))
  show_cast <- tibble::as_tibble(cast_parsed)
  cast <- show_cast |>
    unnest_wider(person, names_sep = "_") |>
    select(person_name, person_gender, person_birthday, person_deathday) |>
    filter(!is.na(person_gender)) |>
    mutate(
      person_birthday = ymd(person_birthday),
      person_deathday = ymd(person_deathday),
      died = ifelse(!is.na(person_deathday),"deceased","alive"),
      age = ifelse(
        !is.na(person_deathday),
        as.integer(difftime(person_deathday, person_birthday) / 365),
        as.integer(difftime(ymd("2024-06-30"), person_birthday) / 365)
      )
    )
  ggplot(cast, aes(x = age, y = person_gender, color = person_gender, shape = died)) +
    geom_point(size = 6) +
    scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
    scale_y_discrete(limits = c("Male", "Female")) +
    scale_shape_manual(values = c("alive" = 79, "deceased" = 88)) +
    labs(
      x = "Age",
      y = "Gender",
      title = "Age Distribution of Cast by Gender and Deceased Status",
      color = "Gender",
      shape = "Status"
    )  

})

})
