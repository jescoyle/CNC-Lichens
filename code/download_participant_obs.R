### This script downloads the following dataset using the iNat API.
###

### Load data and functions used to download data
library(rjson)
library(RCurl)
library(dplyr)
library(tidyr)
library(lubridate)
source("code/download_functions.R")

#####################################################
### Data set: CNC_participant_observations.csv
### Contains all observations from iNaturalist users who observed at least one lichen during a CNC event.

## Load observation data
obs_dat <- read.csv("data/CNC_lichen_obs.csv")

## Find all users who observed a lichen during a CNC
## This will have one row for each participant each year and lumps observations from different localities
participants <- obs_dat %>%
  filter(Period == "during") %>%
  select(Year, starts_with("user"), ) %>%
  unique() %>%
  pivot_wider(names_from = Year, values_from = Year, names_prefix = "Y.") %>%
  mutate(across(starts_with("Y."), ~ if_else(is.na(.x), FALSE, TRUE))) 


# Extract day that user started iNat account
participants$start_date <- as.Date(substr(participants$user.created_at, start = 1, stop = 10))

# Define time periods for querying iNat
# use 13-week quarters with the first one ending at the start of the 2018 CNC
CNCstartdates <- mdy(subset(date_ranges, period == "during")$date_from)

week13dates <- as_date(sapply(CNCstartdates, function(x) x + weeks(c(-13, 0, 13, 26))),
                       origin = lubridate::origin)
week13dates <- c(week13dates, week13dates[5*4] + weeks(13))

# Define intervals between dates
week13ints <- int_diff(week13dates)

## Download all observations from all participants at once in blocks of mutliple participants at a time

## NOTE: manually changed the name of the file to save to (e.g. _1, _2, etc) so that it doesn't get too big.
##       will concatenate later.

# Define blocks of participants to query (can't do all at once)
partblocks <- seq(0, nrow(participants), 380) 
partblocks <- c(partblocks, nrow(participants))

# Define range of observation ids to search
# min and max ids based on manual iNat search for observations entered on the beginning date and up to current maxID
id_range <- c(9600000, 150400000)

# Define number of results per page
resultsperpage <- 200 # max is 200, but query wont send more than 30

# Reset query counter if this is a new day
pings <- c(0, 0)  # start pings at these numbers

# Indicate whether this is a restart in the middle of a file - determines whether to write a new file or append
i <- 1



# Set file number incrementer
fnum <- 4
this_dat_file <- paste0("data/working/CNC_participant_obs_", fnum, ".csv")

# Check where to start based on what has been written to the results file
if(file.exists(this_dat_file)){
  end_line <- system(paste("tail -n 1", this_dat_file), intern = TRUE)
  end_line_vars <- unlist(strsplit(end_line, ","))
  
  i <- as.numeric(end_line_vars[1])
  min_id <- as.numeric(regmatches(end_line_vars[2], regexpr( "[0-9]+", end_line_vars[2])))
  
} else {
  i <- 1
  min_id <- id_range[1]-1
}



# Start timer for rate limiting query
start_time <- c(Sys.time(), Sys.time())
#start_time <- c(Sys.time(),as.POSIXct("2023-03-30 08:25:49 PDT"))

# Loop through blocks of participants
while(i < length(partblocks)){
  
  # Define participants to query
  participant_string <- paste(participants$user.id[(partblocks[i]+1):partblocks[i+1]], collapse = ",")
  
  # Define initial range of id to search over
  increment <- 50000
  
  # Set max_id based on increment
  max_id <- min_id + increment
  
  # Define query
  query_url <- paste0(obs_url,
                      "&user_id=", participant_string,
                      "&d1=", week13dates[1],
                      "&d2=", week13dates[length(week13dates)],
                      "&id_above=", min_id,
                      "&id_below=", max_id,
                      "&per_page=", resultsperpage,
                      "&order_by=id",
                      "&order=asc")
  
  # Get results from query
  results_json <- fromJSON(getURL(query_url))
  
  # if there is an error, wait 2 mins and try one more time
  if(!is.null(results_json$error)){
    Sys.sleep(120) 
    results_json <- fromJSON(getURL(query_url))
  }
  
  # Keep track of number of queries
  pings <- pings + 1
  
  # Extract obs json object to a list of dataframes with only the fields needed
  #   results in a list
  obs_extract <- lapply(results_json$results, extract_obs2df)
  
  # Save total number of results and remove large object
  total_results <- results_json$total_results
  rm(results_json)
  
  # Convert list to dataframe and add on to growing data
  if(total_results > 0){
    participant_obs_dat <- data.frame(Block = i, do.call(rbind, obs_extract)) # converts to dataframe and records the participant block
    lastID <- max(as.numeric(participant_obs_dat$id)) # save the last observation ID recorded for use later
  } else {
    # Set values to indicate that no results were returned
    total_results <- 1 # avoids division by zero on the next loop
    participant_obs_dat <- data.frame()
    lastID <- max_id
  }
  
  print(paste("Block = ", i))
  #print(paste("min = ", min_id))
  #print(paste("max = ", max_id))
  
  # Limit query rate
  if(pings[1] %% 5 == 0){
    tq <- throttle_queries(pings, start_time, rate_limit = 1, day_limit = 10000)
    pings <- tq$pings
    start_time <- tq$start_time
  }
  
  # Save
  if(nrow(participant_obs_dat) > 0){
    if(file.exists("data/CNC_participant_obs_4.csv")){
      # Append to existing results
      write.table(participant_obs_dat, this_dat_file, 
                  row.names = FALSE, 
                  append = TRUE,
                  col.names = FALSE,
                  sep = ",")
    } else {
      # Create a new file if the file doesn't exist
      write.csv(participant_obs_dat, this_dat_file, row.names = FALSE)
    }}
  
  rm(participant_obs_dat, obs_extract) # remove data frame to prevent memory from getting overloaded
  
  # Loop through possible range of observation IDs
  while(max_id < id_range[2]){
    
    # If the query returned more results than are reported
    # reduce the size of the increment and query again using th highest observed ID as the new min ID
    if(total_results > resultsperpage){
      
      min_id <- lastID
      
      # Define new increment to return max results per page assuming the same rate of finding observations
      increment <- ceiling(increment/total_results*resultsperpage)
      max_id <- min_id + increment
      
      # Define query
      query_url <- paste0(obs_url,
                          "&user_id=", participant_string,
                          "&d1=", week13dates[1],
                          "&d2=", week13dates[length(week13dates)],
                          "&id_above=", min_id,
                          "&id_below=", max_id,
                          "&per_page=", resultsperpage,
                          "&order_by=id",
                          "&order=asc")
      
      # Get results from query
      results_json <- fromJSON(getURL(query_url))
      
      # if there is an error, wait 2 mins and try one more time
      if(!is.null(results_json$error)){
        Sys.sleep(120) 
        results_json <- fromJSON(getURL(query_url))
      }
      
      # Keep track of number of queries
      pings <- pings + 1
      
    } else {
      
      min_id <- max_id - 1
      
      # Define new increment to return max results per page assuming the same rate of finding observations
      increment <- ceiling(increment/total_results*resultsperpage)
      max_id <- min_id + increment
      
      # Define query
      query_url <- paste0(obs_url,
                          "&user_id=", participant_string,
                          "&d1=", week13dates[1],
                          "&d2=", week13dates[length(week13dates)],
                          "&id_above=", min_id,
                          "&id_below=", max_id,
                          "&per_page=", resultsperpage,
                          "&order_by=id",
                          "&order=asc")
      
      # Get results from query
      results_json <- fromJSON(getURL(query_url))
      
      # if there is an error, wait 2 mins and try one more time
      if(!is.null(results_json$error)){
        Sys.sleep(120) 
        results_json <- fromJSON(getURL(query_url))
      }
      
      # Keep track of number of queries
      pings <- pings + 1
      
    } # closes if-else statement determining the min and max ids for the next query
    
    # Extract obs json object to a list of dataframes with only the fields needed
    obs_extract <- lapply(results_json$results, extract_obs2df)
    
    # Save total number of results and remove large object
    total_results <- results_json$total_results
    rm(results_json, query_url)
    
    if(total_results > 0){
      participant_obs_dat <- data.frame(Block = i, do.call(rbind, obs_extract)) # converts to dataframe and records the participant block
      lastID <- max(as.numeric(participant_obs_dat$id))  # save the last observation ID recorded for use later
    } else {
      total_results <- 1 # avoids division by zero on the next loop
      participant_obs_dat <- data.frame()
      lastID <- max_id
    }
    
    #print(paste("min = ", min_id))
    #print(paste("max = ", max_id))
    
    # Limit query rate
    if(pings[1] %% 5 == 0){
      tq <- throttle_queries(pings, start_time, rate_limit = 1, day_limit = 10000)
      pings <- tq$pings
      start_time <- tq$start_time
    }
    
    # Append to existing results
    if(nrow(participant_obs_dat) > 0){
      write.table(participant_obs_dat, this_dat_file, 
                  row.names = FALSE, 
                  append = TRUE,
                  col.names = FALSE,
                  sep = ",")
    }
    rm(participant_obs_dat, obs_extract)
    
  } # closes while loop through the observation IDs
  
  # Move on to the next block of participants and reset the minimum obs id
  i <- i + 1
  min_id <- id_range[1]-1
  
  # Check file size and change the file name to the next number if it is too large
  if(file.info(this_dat_file)$size > 400000000){
    fnum <- fnum + 1
    this_dat_file <- paste0("data/working/CNC_participant_obs_", fnum, ".csv")
  }
  
} # closes loop through blocks of participants

