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
View(info)
