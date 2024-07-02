# library(jsonlite)
# https://www.tvmaze.com/api
# after shows we can query for
# /shows/id/cast,episodes,images,crew,seasons
url_id <- "https://api.tvmaze.com/shows"
id_info <- httr::GET(url_id)
str(id_info, max.level = 1)
parsed <- fromJSON(rawToChar(id_info$content))
info <- tibble::as_tibble(parsed)
View(info)

# lets try and search by name and then get the rest of the information
BreakingBad <- subset(info, name == "Breaking Bad")
id <- BreakingBad$id
id
Parks <- subset(info, name == "Parks and Recreation")
id <- Parks$id
id

url_id <- "https://api.tvmaze.com/shows/174/episodes"
id_info <- httr::GET(url_id)
str(id_info, max.level = 1)
parsed <- fromJSON(rawToChar(id_info$content))
info <- tibble::as_tibble(parsed)
library(tidyr)
episode_info <- info |>
  unnest_wider(rating, names_sep = "_") |>
  select(name, season, number, type, airdate, runtime, rating_average)
View(episode_info)

search_show <- function(url = "https://api.tvmaze.com/shows", show_name, endpoint){
  id_info <- httr::GET(url)
  parsed <- fromJSON(rawToChar(id_info$content))
  all_shows <- tibble::as_tibble(parsed)
    if (!is.null(name)){
    subsetted <- subset(all_shows, name == show_name)
    id <- subsetted$id
    new_url <- paste(url, id, endpoint, sep = "/")
    id_info_specific <- httr::GET(new_url)
    parsed_specific <- fromJSON(rawToChar(id_info_specific$content))
    info_specific <- tibble::as_tibble(parsed_specific)
    return(info_specific)
    }
  else{
    return(all_shows)
  }
  }

data <- search_show(url = "https://api.tvmaze.com/shows", show_name = "Breaking Bad", endpoint = "episodes")  
View(data)
url <- "https://api.tvmaze.com/shows"
id_info <- httr::GET(url)
parsed <- fromJSON(rawToChar(id_info$content))
all_shows <- tibble::as_tibble(parsed)
colnames(all_shows)


#############################################
