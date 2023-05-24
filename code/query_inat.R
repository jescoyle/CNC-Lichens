# This script queries iNaturalist API for the data needed for the project

library(rjson)
library(RCurl)
library(dplyr)
library(tidyr)
library(lubridate)

# Set up new class for reading in numbers with commas
setClass("num.with.commas")
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )

##################################################
### Get place ids for cities to query based on 
### those that participated in CNC

# Define cities participating
# Data were copied from the following websites and pasted into the CNC_cities.csv spreadsheet
# 2018: https://citynaturechallenge.org/leaderboard-2018/
# 2019: https://citynaturechallenge.org/leaderboard-2019/
# 2020: https://citynaturechallenge.org/collective-results-2020/
# 2021: https://citynaturechallenge.org/collective-results-2021/
# 2022: https://citynaturechallenge.org/collective-results-2022/

cities <- read.csv("data/CNC_cities.csv", 
                   stringsAsFactors= FALSE, 
                   fileEncoding = "UTF-8",
                   colClasses = c("numeric", "character", "num.with.commas", "num.with.commas", "num.with.commas"))

# Drop cities with 0 participants
cities <- filter(cities, People > 0)


# Examine the number of cities with various numbers of participants
cities %>%
  mutate(People_bins = cut(People, breaks = c(1, 5, 20, 50, 75, 100, 200, 500, max(People)+1), right = FALSE)) %>%
  group_by(Year, People_bins) %>%
  summarize(N = n()) %>%
  pivot_wider(names_from = Year, values_from = N)

# Seems like limiting the analysis to cities with at least 100 is reasonable
minPeople <- 100
city_names <- unique(filter(cities, People >= minPeople)$City)

cities <- filter(cities, City %in% city_names)


cities %>%
  group_by(Year) %>%
  summarise(Num_cities = n())
# Year Num_cities
# <dbl>      <int>
# 1  2018         43
# 2  2019         68
# 3  2020        102
# 4  2021        131
# 5  2022        155

# Write out list of cities to edit manually
#write.csv(cities, paste0("data/CNC_cities_minPeople", minPeople, ".csv"), 
#          row.names = FALSE, fileEncoding = "UTF-8")



# Manual edits: 
# Added column that identifies a participating location when the city name changes across years
# Added columns to identify the PlaceIDs used by iNaturalist to include observations in
#   each local CNC project.


#######################################################
### Get taxon ids that correspond to lichen Genera the CNALH Global Checklist 
### downloaded 2023-02-01 from https://lichenportal.org/portal/checklists/checklist.php?clid=1492&pid=558
### downloaded csv file without synonyms or notes.



# Define lichen-forming fungal taxa
lichen_taxa <- read.csv("data/CNALH Global Checklist 20230201.csv")

# Generate dataframe with just the genus and family
Genus <- sapply(lichen_taxa$ScientificName, function(x) unlist(strsplit(x, " "))[1]); names(Genus) <- NULL
Family <- sub("([[:alpha:]])", "\\U\\1", tolower(lichen_taxa$Family), perl = TRUE)
lichen_taxa <- data.frame(Family, Genus, Authority = lichen_taxa$ScientificNameAuthorship)

# Load full taxonomy from iNaturalist downloaded on 2023-02-01
iNat_taxa <- read.csv("data/working/inaturalist-taxonomy.dwca_20230201/taxa.csv")

# Subset to only Ascomycota and Basidiomycota
iNat_fungi <- subset(iNat_taxa, phylum %in% c("Ascomycota", "Basidiomycota"))

# Generate list of lichen-forming fungal taxa in iNaturalist
iNat_lichens <- subset(iNat_fungi, (genus %in% lichen_taxa$Genus))

# Match with CNALH global checklist to identify which lichen taxa are not in iNat
iNat_lichenGenera <- subset(iNat_lichens, taxonRank == "genus")
match_lichen_genera <- left_join(lichen_taxa, iNat_lichenGenera, by = c("Genus" = "genus"))

# Which don't match?
nomatch <- subset(match_lichen_genera, is.na(id))
sort(nomatch$Genus) # Nothing worrysome here. 

# Which had duplicate IDS in iNat?
multiplematchgenera <- unique(match_lichen_genera[duplicated(match_lichen_genera$Genus), 'Genus'])
subset(match_lichen_genera, Genus %in% multiplematchgenera)

# only one mismatch: Echinoplaca has two ids in iNat b/c it is assigned to two different families
# keep only the id that matches the CNALH checklist family
match_lichen_genera <- filter(match_lichen_genera, id != 175849)
sum(is.na(match_lichen_genera$id)) # this also filters out the missing data


# Save table of lichen genera and ids used
#write.csv(match_lichen_genera, "data/lichen_genera.csv", row.names = FALSE)



#######################################################
### Download observation IDs for each city each year they participated into a data table.
### Must limit download rate from API to 60 per minute and 10000 per day
### Run this section before generating any of the three data sets below

source("code/download_functions.R")


######################################################
### Data set: CNC_lichen_obs.csv
### Lichen observations from participating CNC cities 
### before, during and after CNC 2018 - 2022

## Loop through cities dataframe and save observations in a dataframe
## Could be done more efficiently by finding all place id at once, but this is easier

# Define which rows to do
Nmin <- 488
Nmax <- 556# should eventually cover all rows

# Define number of results per page
resultsperpage <- 30 # max is 200, but query wont send more than 30

pings <- c(0, 0)

# Read in existing data to add on to
if(file.exists("data/CNC_lichen_obs.csv")){
  obs_dat <- read.csv("data/CNC_lichen_obs.csv")
} else { 
  obs_dat <- data.frame()
}

# Check which years and cities have been done - this will also ID cities with no records
finished_cities <- obs_dat %>%
  group_by(Year, City) %>%
  summarize(N = n())


cities %>%
  select(Year, City) %>%
  left_join(finished_cities)

# Set timer for limiting query rate
start_time <- rep(Sys.time(), 2)

for(i in Nmin:Nmax){
  
  # Define the parameters for this CNC event
  this_city <- cities[i, "City"]
  this_year <- cities[i, "Year"]
  place_ids_string <- gsub(" ", ",", cities[i, "PlaceID"])
  
  for(p in c("before", "during", "after")){
    
    # Define page number to retrieve 
    page_num <- 1
    total_results <- Inf
    
    # Look through multiple pages of results
    while(total_results - (page_num - 1)*resultsperpage > 0){
      query_url <- paste0(obs_url, 
                      "place_id=", place_ids_string,
                      "&taxon_id=", taxon_ids_string,
                      "&d1=", subset(date_ranges, year == this_year & period == p)$date_from,
                      "&d2=", subset(date_ranges, year == this_year & period == p)$date_to,
                      "&page=", page_num,
                      "&per_page", resultsperpage)
      results_json <- fromJSON(getURL(query_url))
    
      total_results <- results_json$total_results # Number of results (used to determine number of pages needed to query)
      
      # Keep track of number of queries
      pings <- pings + 1
      
      # If there are any results
      if(total_results > 0){
      
        # Extract obs json object to a list of dataframes with only the fields needed
        obs_extract <- lapply(results_json$results, extract_obs2df)
        
        # Convert list to dataframe
        obs_extract <- as.data.frame(do.call(rbind, obs_extract))
        
        # Add on columns identifying the CNC parameters
        obs_dat <- rbind(obs_dat,
                          data.frame(Year = this_year, City = this_city, Period = p, obs_extract))
        
      } # closes if statement for no results
      
      # Limit query rate
      tq <- throttle_queries(pings, start_time, rate_limit = 1, day_limit = 10000)
      pings <- tq$pings
      start_time <- tq$start_time
      
      # Increment the page number of results to retrieve (for next iteration of while loop)
      page_num <- page_num + 1
      
      
    } # closes while loop of pages of results
    
  } # closes period loop

  write.csv(obs_dat, "data/CNC_lichen_obs.csv", row.names = FALSE) # uncomment when running
  print(paste("Done with row",i, ":", this_year, this_city))
  
} # closes loop through cities dataframe






######################################################
### Data set: CNC_obs_count.csv
### Count of observations from participating CNC cities 
### before, during and after CNC 2018 - 2022

## Loop through cities dataframe and save observations in a dataframe
## Could be done more efficiently by finding all place id at once, but this is easier

# Define which rows to do
Nmin <- 488
Nmax <- 556 # should eventually cover all rows: 

# Define number of results per page
resultsperpage <- 30 # max is 200, but query wont send more than 30

pings <- c(0,0)

# Read in existing data to add on to
if(file.exists("data/CNC_obs_count.csv")){
  obs_count <- read.csv("data/CNC_obs_count.csv")
} else { 
  obs_count <- data.frame()
}

# Set timer for limiting query rate
start_time <- rep(Sys.time(), 2)

for(i in Nmin:Nmax){
  
  # Define the parameters for this CNC event
  this_city <- cities[i, "City"]
  this_year <- cities[i, "Year"]
  place_ids_string <- gsub(" ", ",", cities[i, "PlaceID"])
  
  for(p in c("before", "during", "after")){
    
    query_url <- paste0(obs_url, 
                        "place_id=", place_ids_string,
                        "&d1=", subset(date_ranges, year == this_year & period == p)$date_from,
                        "&d2=", subset(date_ranges, year == this_year & period == p)$date_to,
                        "&only_id=TRUE")
    results_json <- fromJSON(getURL(query_url))
    
    total_results <- results_json$total_results # Number of results (used to determine number of pages needed to query)
    
    # Keep track of number of queries
    pings <- pings + 1
    
    # Add to obs_count dataframe
    obs_count <- rbind(obs_count, 
                       data.frame(Year = this_year, City = this_city, Period = p, Observations.All = total_results))
    
    
    # Limit query rate
    tq <- throttle_queries(pings, start_time, rate_limit = 1, day_limit = 10000)
    pings <- tq$pings
    start_time <- tq$start_time
    
  } # closes period loop
  
  #write.csv(obs_count, "data/CNC_obs_count.csv", row.names = FALSE) #uncomment when running
  print(paste("Done with row",i, ":", this_year, this_city))
  
} # closes loop through cities dataframe




#### WORKING HERE: download moved to new script: download_participant_obs.R


## NOTE: Loop probably recorded duplicate records, so need to remove duplicates before analyzing
## This is because participants contains duplicate records that differ only by the number of 
## identifications and observations the user made (because records in obs_dat were downloaded at different times
## Need to query all participants to get more up to date total observations.
# e.g: 
which(duplicated(participants$user.id))
participants[3379,]
filter(obs_dat, user.id == 112578)



## Check whether any records were missed because they were outside the range searched

query_url <- paste0(obs_url,
                    "&user_id=", participant_string,
                    "&d1=", week13dates[1],
                    "&d2=", week13dates[length(week13dates)],
                    "&id_below=", id_range[1],
                    "&per_page=", resultsperpage,
                    "&order_by=id",
                    "&order=asc")
# Get results from query
results_json <- fromJSON(getURL(query_url))

query_url <- paste0(obs_url,
                    "&user_id=", participant_string,
                    "&d1=", week13dates[1],
                    "&d2=", week13dates[length(week13dates)],
                    "&id_above=", id_range[2],
                    "&per_page=", resultsperpage,
                    "&order_by=id",
                    "&order=asc")
# Get results from query
results_json <- fromJSON(getURL(query_url))





# NO LONGER DOWNLOADING USING THIS METHOD
# # Define which rows to do
# Nmin <- 502
# Nmax <- nrow(participants) # should eventually cover all rows
# 
# # Define number of results per page
# resultsperpage <- 200 # max is 200, but query wont send more than 30
# 
# pingcounter_min <- 0
# pingcounter_day <- 0
# 
# # Read in existing data to add on to
# if(file.exists("data/CNC_participant_obs_count.csv")){
#   participant_obs <- read.csv("data/CNC_participant_obs_count.csv")
#   participant_obs$start.date <- as.Date(participant_obs$start.date)
#   participant_obs$end.date <- as.Date(participant_obs$end.date)
# } else {
#   participant_obs <- data.frame()
# }
# 
# # For each participant:
# for(i in Nmin:Nmax){
# 
# 
#   # Find the time interval during which they joined iNat
#   # If they joined on the date that bounds an interval use the subsequent interval
#   start_int <- which(participants$start_date[i] %within% week13ints)
#   start_int <- ifelse(length(start_int) > 0, max(start_int), 1) # This takes care of the case when a user joined prior to the time intervals
# 
#   # Determine which intervals to query
#   use_dates <- week13dates[start_int:length(week13dates)]
# 
#   for(j in 1:(length(use_dates)-1)){
# 
#     # Count the number lichen observations they made during this interval
#     query_url <- paste0(obs_url,
#                         "&user_id=", participants[i, "user.id"],
#                         "&d1=", use_dates[j],
#                         "&d2=", use_dates[j+1] - days(1),
#                         "&taxon_id=", taxon_ids_string,
#                         "&only_id=TRUE")
#     results_json <- fromJSON(getURL(query_url))
# 
#     # if there is an error, wait 2 mins and try one more time
#     if(!is.null(results_json$error)){
#       Sys.sleep(120)
#       pingcounter_min <- 0
#       results_json <- fromJSON(getURL(query_url))
#     }
# 
#     total_lichen_results <- results_json$total_results # Number of results (used to determine number of pages needed to query)
# 
#     # Count the number lichen observations they made during this interval
#     query_url <- paste0(obs_url,
#                         "&user_id=", participants[i, "user.id"],
#                         "&d1=", use_dates[j],
#                         "&d2=", use_dates[j+1] - days(1),
#                         "&only_id=TRUE")
#     results_json <- fromJSON(getURL(query_url))
# 
#     # if there is an error, wait 2 mins and try one more time
#     if(!is.null(results_json$error)){
#       Sys.sleep(120)
#       pingcounter_min <- 0
#       results_json <- fromJSON(getURL(query_url))
#     }
# 
#     total_results <- results_json$total_results # Number of results (used to determine number of pages needed to query)
# 
# 
#     # Add to obs_count dataframe
#     participant_obs <- rbind(participant_obs,
#                        data.frame(user.id = participants[i, "user.id"],
#                                   start.date = use_dates[j],
#                                   end.date = use_dates[j+1] - days(1),
#                                   n_obs = total_lichen_results,
#                                   n_obs_all = total_results))
# 
# 
#     # Keep track of number of queries per minute (limit = 60) and per day (limit = 10000)
#     pingcounter_min <- pingcounter_min + 2
#     pingcounter_day <- pingcounter_day + 2
# 
#     # Print the current number of queries
#     if(pingcounter_min %% 10 == 0) print(paste("Queries this minute:", pingcounter_min, "   Queries this day:", pingcounter_day))
# 
#     # Limit query rate
#     if(pingcounter_min >= 60){
#       Sys.sleep(60)
#       pingcounter_min <- 0
#     }
#     if(pingcounter_day >= 10000){
#       print(paste("Daily query exceeds max at", Sys.time()))
#       break
#     }
#   } # closes loop through date intervals
# 
# 
#   # Save and print status
#   write.csv(participant_obs, "data/CNC_participant_obs_count.csv", row.names = FALSE)
# 
#   # Report status
#   print(paste("Done with row", i))
# 
# } # closes loop through participants




###### WORKING HERE #######

## Determine whether participant was a veteran, novice or new iNat user


# Define dates for determining status: 
# new = within 1 week of and during event CNC (cutoff = 3 days after the start date)
# novice = within 50 weeks but earlier than one week (cutoff = 7 days before start date)
# veteran = ealier than 50 weeks (cutoff = 350 days before start date)
CNC2019_start <- as.Date(subset(date_ranges, year == 2019 & period == "during")$date_from)
CNC2019_end <- as.Date(subset(date_ranges, year == 2019 & period == "during")$date_to)
user_status_dates <- CNC2019_start - c(-3, 7, 350)
names(user_status_dates) <- c("new", "novice", "veteran")

# Check whether each user's start date is before each of the cut-off dates for each status
CNC_CA.2019.participants$user.status <- sapply(CNC_CA.2019.participants$start_date, function(x){
  
  # Check whether each user's start date is before each of the cut-off dates for each status
  earlier_dates <- which(user_status_dates >= x)
  
  # Choose the status corresponding to the one that is the earliest
  names(earlier_dates[length(earlier_dates)])
})




