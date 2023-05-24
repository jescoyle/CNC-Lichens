### This script contains variables and functions used to download and process observations from the iNat API

# Read in manually generated table of cities
cities <- read.csv("data/CNC_cities_minPeople100_withLocality.csv", fileEncoding = "UTF-8")


# Define date ranges for pre-during-post CNC events for 2018 - 2022
date_ranges <- read.csv("data/time_periods.csv")


# Read in lichen genera taxon ids
lichen_genera_df <- read.csv("data/lichen_genera.csv")

# Define lichen taxa
lichen_genera_ids <- na.omit(lichen_genera_df$id)
taxon_ids_string <- paste0(lichen_genera_ids, collapse = ",")


# Define API URL for observations endpoint
obs_url <- "https://api.inaturalist.org/v1/observations?"


### A function that gets the necessary fields from a json observations download
### x = a list item in the $results slot of an object returned by getJSON query to the iNat API
extract_obs2df <- function(x){
  
  # Create text string of place IDs separated by whitespace
  place.id <- paste(x$place_ids, collapse = " ")
  
  # Determine whether observation has coordinates
  if(is.null(x$location)){
    location.lat <- NA
    location.lon <- NA
  } else {
    location.lat = strsplit(x$location, split =",")[[1]][1]
    location.lon = strsplit(x$location, split =",")[[1]][2]
  }
  
  # A function that checks whether a value is null and returns NA if so
  ifNULLsetNA <- function(x){
    ifelse(is.null(x), NA, x)
  }
  
  # Create row of data for this observation with extracted fields
  c(id = x$id,
    observed_on = ifNULLsetNA(x$observed_on),
    observed_time = ifNULLsetNA(x$time_observed_at),
    taxon.id = ifNULLsetNA(x$taxon$id),
    taxon.name = ifNULLsetNA(x$taxon$name),
    taxon.rank = ifNULLsetNA(x$taxon$rank),
    quality_grade = ifNULLsetNA(x$quality_grade),
    user.id = ifNULLsetNA(x$user$id),
    user.login = ifNULLsetNA(x$user$login),
    user.observations_count = ifNULLsetNA(x$user$observations_count),
    user.identifications_count = ifNULLsetNA(x$user$identifications_count),
    user.created_at = ifNULLsetNA(x$user$created_at),
    location.lat = location.lat,
    location.lon = location.lon,
    mappable = ifNULLsetNA(x$mappable),
    place.id = place.id
  )
  
}

# Define function for limiting query rate
# pings is a vector length 2 with a count per minute and a count per day
# start_time is a vector of dates of length 2
# Rate limit is per second.
throttle_queries <- function(pings, start_time, rate_limit = 1, day_limit = 10000){
  
  check_time <- Sys.time()
  secs_passed <- difftime(check_time, start_time, units = "secs")
  
  if(pings[2] %% 50 == 0){
    print(paste("Queries this day:", pings[2]))
    gc()
  }
  
  if(pings[1] > secs_passed[1]*rate_limit){
    Sys.sleep(5*rate_limit)
  }
  
  if((secs_passed[2] < 60*60*24) & (pings[2] > day_limit)){
    print(paste("Daily query exceeds max at", check_time))
    break
  }
  
  # If more than one day has elapsed since the start time, reset the counter
  if(secs_passed[2] > 60*60*24){
    pings[2] <- 0
    start_time[2] <- check_time
  }
  
  rm(check_time, secs_passed)
  
  # Return the number of queries and the start_time
  data.frame(pings = pings, start_time = start_time)
  
}
