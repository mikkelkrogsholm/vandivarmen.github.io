library(tidyverse)

json_files <- list.files("data", full.names = TRUE)

parsed_data <- map_dfr(json_files, function(json_file){
  
  x <- jsonlite::fromJSON(json_file)
  
  features <- x$features
  
  coords <- features$features$geometry$coordinates %>% 
    map_dfr(function(x){
      x %>% 
        as_tibble() %>%
        mutate(coord = c("lng", "lat")) %>%
        spread(coord, value)
    })
  
  props <- features$features$properties %>%
    select(name = layerName) %>%
    as_tibble()
  
  out <- bind_cols(coords, props)
  
  return(out)
  
})

adressedata <- map_dfr(1:nrow(parsed_data), function(i){
  
  lng <- parsed_data$lng[i]
  lat <- parsed_data$lat[i]
  url <- glue::glue("https://dawa.aws.dk/adgangsadresser/reverse?x={lng}&y={lat}&struktur=mini")
  
  data <- jsonlite::fromJSON(url)
  
  data[c("vejnavn", "husnr", "postnr", "postnrnavn", "kommunekode")] %>%
    as_tibble()
  
})


out <- parsed_data %>%
  bind_cols(adressedata)


out %>%
  jsonlite::toJSON(pretty = TRUE) %>%
  write_file("data/waterposts.json")

out %>%
  write_csv("data/waterposts.csv")
