# This script queries iNaturalist API for the data needed for the project

library(rjson)
library(RCurl)
library(dplyr)

##################################################
### Get place ids for cities to query based on 
### those that participated in CNC

# Define cities participating and cities in California
# 2019 and 2020 were done at separate times
#cities <- read.csv("data/CNC2019_cities.csv", stringsAsFactors= FALSE) # 2019
cities <- read.table("data/CNC2020_cities.txt", header = TRUE, sep = "\t", 
                     fileEncoding = "UTF-8", stringsAsFactors = FALSE) # 2020
cities$City_name <- sapply(cities$City, function(x) unlist(strsplit(as.character(x), split = ","))[1])

#CA_cities <- cities[grep("CA", cities$City), ] # 2019
CA_cities <- cities[grep("California, USA", cities$City), ] # 2020



# Get place ids for cities
CA_cities_ids <- vector("list", nrow(CA_cities))
names(CA_cities_ids) <- CA_cities$City_name   

# AUTOMATED LOOKUP NOT QUITE AUTOMATIC
for(p in CA_cities$City_name){

  place_url <- paste0("https://api.inaturalist.org/v1/places/autocomplete?q=", p)
  
  place_dat <- fromJSON(getURL(URLencode(place_url)))
  
  found_places <-  sapply(place_dat$results, function(x){
    matches <- length(grep(p, x$display_name)) > 0
    id <- x$id[matches]

    id
    })
 
  found_place_ids <- unlist(found_places) # drops non-matches
  
  CA_cities_ids[[p]] <- found_place_ids

}

# Look up individual place ids to determine which is correct
place_dat <- fromJSON(getURL("https://api.inaturalist.org/v1/places/121516"))
place_dat$results[[1]]$display_name

# Manually choose correct ones for San Diego and SF Bay Area
CA_cities_ids['Inland Empire'] <- 118665
CA_cities_ids['Los Angeles County'] <- 962 
CA_cities_ids['Mendocino County'] <- 436
CA_cities_ids['Orange County'] <- 2738
CA_cities_ids['Sacramento'] <- 139279
CA_cities_ids['San Diego County'] <- 829
CA_cities_ids['San Francisco Bay Area'] <- 54321


# Add to dataframe
CA_cities$place_id <- unlist(CA_cities_ids)
CA_cities$Year <- 2020

# Save data frame (this was done first with only 2019 data
#write.csv(CA_cities, "data/CNC_cities_CA_2019.csv", row.names = FALSE)

# Read 2019 in city data
CA_cities_2019 <- read.csv("data/CNC_cities_CA_2019.csv", stringsAsFactors = FALSE)
CA_cities_2019$Year <- 2019

# Manually reset Sacramento for consistency
CA_cities_2019[CA_cities_2019$City_name == "Sacramento Region", "City_name"] <- "Sacramento"

colnames(CA_cities_2019) == colnames(CA_cities)

# Combine city data from both years
CA_cities <- rbind(CA_cities, CA_cities_2019)

# Save combined data
#write.csv(CA_cities, "data/CNC_cities_CA_2019-2020.csv", row.names = FALSE)

#######################################################
### Get taxon ids that correspond to lichen Genera in Esslinger Checklist 
### downloaded 2020-04-18 from https://www.ndsu.edu/pubweb/~esslinge/chcklst/chcklst7.htm

# Define lichen-forming fungal taxa
#lichen_taxa_raw <- scan(file = "data/Esslinger_checklist_v23_2020-04-18.txt",
#                        what = character(),
#                        blank.lines.skip = TRUE)
#lichen_genera <- lichen_taxa_raw[grep("[A-Z]{3,}", lichen_taxa_raw)]

# Sort alphabetically and remove duplicates
#lichen_genera <- sort(lichen_genera)
#lichen_genera <- lichen_genera[!duplicated(lichen_genera)]

#lichen_genera_ids <- sapply(lichen_genera, function(g){
  
  
#lichen_genera_ids <- vector("numeric", length = length(lichen_genera))
#names(lichen_genera_ids) <- lichen_genera

#for(g in lichen_genera){
#  taxon_url <- paste0("https://api.inaturalist.org/v1/taxa?q=", g,
#                    "&rank=genus&only_id=TRUE")
#  taxon_dat <- fromJSON(getURL(taxon_url))
#  
#  taxon_id <- unlist(taxon_dat$results)
#  n_ids <- length(taxon_id)
#  lichen_genera_ids[g] <- ifelse(n_ids == 0, NA, taxon_id)
#  
#  # Queries are rate-limited to 100 per minute
#  if(which(lichen_genera==g) %% 100 == 0){
#    print(which(lichen_genera==g))
#    Sys.sleep(60)
#  }
#}

# Save table of lichen genera and ids used
#id_lens <- sapply(lichen_genera_ids, length)
#lichen_genera_ids[id_lens > 1]

#lichen_genera_df <- data.frame(Genus = names(lichen_genera_ids), taxon_id = lichen_genera_ids)
#write.csv(lichen_genera_df, "data/lichen_genera.csv", row.names = FALSE)




# Define date ranges for pre-during-post 2019 and 2020 events
date_ranges <- read.csv("data/time_periods.csv")


# Read in lichen genera taxon ids
lichen_genera_df <- read.csv("data/lichen_genera.csv")

# Define lichen taxa
lichen_genera_ids <- na.omit(lichen_genera_df$taxon_id)
taxon_ids_string <- paste0(lichen_genera_ids, collapse = ",")

# Define API URL for observations endpoint
obs_url <- "https://api.inaturalist.org/v1/observations?"

# Read in summary for CNC CA cities
CA_cities <- read.csv("data/CNC_cities_CA_2019-2020.csv")

### A function that gets the necessary fields from a json observations download
### x = a list item in the $results slot of an object returned by getJSON query to the iNat API
extract_obs2df <- function(x){
  
  # Define vector of CA city ids
  CA_city_ids <- unique(CA_cities[, c("City_name", "place_id")])
  
  # Determine whether observation is from a CNC city
  if(sum(x$place_ids %in% CA_city_ids$place_id) > 0){
    place.id <- x$place_ids[which(x$place_ids %in% CA_city_ids$place_id)]
    city <- CA_city_ids[CA_city_ids$place_id == place.id, "City_name"] 
  } else {
    place.id <- NA
    city <- NA
  }
  
  # Determine whether observation has coordinates
  if(is.null(x$location)){
    location.lat <- NA
    location.lon <- NA
  } else {
    location.lat = strsplit(x$location, split =",")[[1]][1]
    location.lon = strsplit(x$location, split =",")[[1]][2]
  }
  
  # Create row of data for this observation with extracted fields
  data.frame(id = x$id,
             user.id = x$user$id,
             user.login = x$user$login,
             user.observations_count = x$user$observations_count,
             user.identifications_count = x$user$identifications_count,
             user.created_at = x$user$created_at,
             observed_on = x$observed_on,
             place.id = place.id,
             #place.location = x$place_guess,
             city = city,
             location.lat = location.lat,
             location.lon = location.lon,
             mappable = x$mappable,
             taxon.id = x$taxon$id,
             taxon.name = x$taxon$name,
             taxon.rank = x$taxon$rank,
             quality_grade = x$quality_grade)
  
}


######################################################
### Data set 1: CNC_CA.csv
### Lichen observations from participating CA cities 
### during 2019 and 2020 CNCs

# Find the number of observations during each event
CNC_nobs <- sapply(2019:2020, function(y){
  
  # Define places
  place_ids_string <- paste0(subset(CA_cities, Year == y)$place_id, collapse=",")

  query_url <- paste0(obs_url, 
                      "place_id=", place_ids_string,
                      "&taxon_id=", taxon_ids_string,
                      "&d1=", subset(date_ranges, year == y & period == "during")$date_from,
                      "&d2=", subset(date_ranges, year == y & period == "during")$date_to,
                      "&only_id=TRUE")
  results_json <- fromJSON(getURL(query_url))
  
  # Report number of observations
  results_json$total_results
  
})
names(CNC_nobs) <- 2019:2020

# Use a loop to extract obervation ids from all records across multiple pages
CNC_CA <- data.frame()

for(y in 2019:2020){
  
  # Define places
  place_ids_string <- paste0(subset(CA_cities, Year == y)$place_id, collapse=",")
  
  # Define date range for this period
  date_from <- subset(date_ranges, year == y & period == "during")$date_from
  date_to <- subset(date_ranges, year == y & period == "during")$date_to
  
  # Calculate number of pages of records
  num_records <- CNC_nobs[as.character(y)]
  num_pages <- num_records %/% 30
  if(num_records %% 30 > 0) num_pages <- num_pages + 1
  
  if(num_pages > 0){
  for(i in 1:num_pages){
    query_page_url <- paste0(obs_url, 
                             "place_id=", place_ids_string,
                             "&taxon_id=", taxon_ids_string,
                             "&d1=", date_from,
                             "&d2=", date_to,
                             "&page=",i)
    
    obs <- fromJSON(getURL(query_page_url))

    # Extract obs json object to a list of dataframes with only the fields needed
    obs_extract <- lapply(obs$results, extract_obs2df)
                          
    # Convert list to dataframe
    obs_extract <- as.data.frame(do.call(rbind, obs_extract))
    
    # Add to all data
    CNC_CA <- rbind(CNC_CA, obs_extract)
    
    if(i %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
  }}
}

#write.csv(CNC_CA, "data/CNC_CA.csv", row.names = FALSE)


#####################################################
### Data set 2: prepostCNC_CA
### Lichen observations from CA before, during and 
### after the 2019 CNCs

# Create a dataframe for the time periods to query
query_periods <- date_ranges

## Get place id for California
place_dat <- fromJSON(getURL(URLencode("https://api.inaturalist.org/v1/places/autocomplete?q=California")))
  
found_places <-  sapply(place_dat$results, function(x){
    matches <- length(grep("California", x$display_name)) > 0
    id <- x$id[matches]

    id
})
found_place_ids <- unlist(found_places) 

place_dat <- fromJSON(getURL(URLencode(paste0("https://api.inaturalist.org/v1/places/", 
                                              paste0(found_place_ids, collapse = ",")))))

CA.id <- place_dat$results[[which(sapply(place_dat$results, function(x) x$display_name)=="California, US")]]$id
# It is 14

# Find the number of lichen observations in CA during each time period
query_periods$CA_nobs <- sapply(1:nrow(date_ranges), function(i){

  query_url <- paste0(obs_url,
                      "place_id=", CA.id,
                      "&taxon_id=", taxon_ids_string,
                      "&d1=", query_periods[i, "date_from"],
                      "&d2=", query_periods[i, "date_to"],
                      "&only_id=TRUE")
  results_json <- fromJSON(getURL(query_url))
  
  # Report number of observations
  results_json$total_results
  
})


# Use a loop to extract obervation ids from all records across multiple pages
prepostCNC_CA <- data.frame()

for(j in 1:nrow(query_periods)){
  
  # Define date range for this period
  date_from <- query_periods[j, "date_from"]
  date_to <- query_periods[j, "date_to"]
  
  # Calculate number of pages of records
  num_records <- query_periods[j, "CA_nobs"]
  num_pages <- num_records %/% 30
  if(num_records %% 30 > 0) num_pages <- num_pages + 1
  
  if(num_pages > 0){
    for(i in 1:num_pages){
      query_page_url <- paste0(obs_url, 
                               "place_id=", CA.id,
                               "&taxon_id=", taxon_ids_string,
                               "&d1=", date_from,
                               "&d2=", date_to,
                               "&page=",i)
      
      obs <- fromJSON(getURL(query_page_url))
      
      # Extract obs json object to a list of dataframes with only the fields needed
      obs_extract <- lapply(obs$results, extract_obs2df)
      
      # Convert list to dataframe
      obs_extract <- as.data.frame(do.call(rbind, obs_extract))
      
      # Add to all data
      prepostCNC_CA <- rbind(prepostCNC_CA, obs_extract)
      
      if(i %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
    }}
}


#write.csv(prepostCNC_CA, "data/prepostCNC_CA.csv", row.names = FALSE)


#####################################################
### Data set 3: CNC2019_participants.csv, CNC2019_obs_veteran.csv, CNC2019_obs_novice.csv, CNC2019_obs_new.csv
### Lichen observations from CA lichen-observing 
### participants of the 2019 CNC within 1 year of
### of the event


## Load CNC CA data
CNC_CA <- read.csv("data/CNC_CA.csv")

# Add a column identifying the event year
CNC_CA$year <- format(as.Date(CNC_CA$observed_on, format = "%Y-%m-%d"), "%Y")

# Subset to 2019
CNC_CA.2019 <- subset(CNC_CA, year == "2019")

## Find all participants in 2019
CNC_CA.2019.participants <- unique(select(CNC_CA.2019, starts_with("user")))


## Determine whether participant was a veteran, novice or new iNat user

# Extract day that user started iNat account
CNC_CA.2019.participants$start_date <- as.Date(substr(CNC_CA.2019.participants$user.created_at, start = 1, stop = 10))

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


## Define sampling time periods within which to examine observations by each user based on user status

# Set up array to hold before/after CNC begin and end dates
user_dates <- array(NA, dim = c(nrow(CNC_CA.2019.participants), 2, 2), 
                    dimnames = list(CNC_CA.2019.participants$user.id, 
                                    c("beforeCNC.2019", "afterCNC.2019"),
                                    c("date_from", "date_to")))

# Set dates for veterans to be 50 weeks before and after the event with a 1-week buffer
user_dates[CNC_CA.2019.participants$user.status == "veteran", "beforeCNC.2019", "date_from"] <- as.character(CNC2019_start - (7*50))
user_dates[CNC_CA.2019.participants$user.status == "veteran", "beforeCNC.2019", "date_to"] <- as.character(CNC2019_start - 7)
user_dates[CNC_CA.2019.participants$user.status == "veteran", "afterCNC.2019", "date_from"] <- as.character(CNC2019_end + 7)
user_dates[CNC_CA.2019.participants$user.status == "veteran", "afterCNC.2019", "date_to"] <- as.character(CNC2019_end + (7*50))

# Set dates for novices based on the number of days before the event that they joined iNat
novice_start_dates <- CNC_CA.2019.participants[CNC_CA.2019.participants$user.status == "novice", "start_date"]
ndays <- CNC2019_start - novice_start_dates
user_dates[CNC_CA.2019.participants$user.status == "novice", "beforeCNC.2019", "date_from"] <- as.character(novice_start_dates)
user_dates[CNC_CA.2019.participants$user.status == "novice", "beforeCNC.2019", "date_to"]  <- as.character(CNC2019_start - 1)
user_dates[CNC_CA.2019.participants$user.status == "novice", "afterCNC.2019", "date_from"] <- as.character(CNC2019_end + 1)
user_dates[CNC_CA.2019.participants$user.status == "novice", "afterCNC.2019", "date_to"]  <- as.character(CNC2019_end + ndays)

# Set dates for newbies to be 50 weeks after the event (no buffer)
user_dates[CNC_CA.2019.participants$user.status == "new", "afterCNC.2019", "date_from"] <- as.character(CNC2019_end + 1)
user_dates[CNC_CA.2019.participants$user.status == "new", "afterCNC.2019", "date_to"] <- as.character(CNC2019_end + (7*50))

## Count total number of observations made by each user in the time periods used in this study 
## and set by user_dates

tot_obs <- data.frame()
nqs <- 0 # keep track of number of queries for rate limiting

for(id in CNC_CA.2019.participants$user.id){
  
  # Find the number of observations 
  n_obs <- sapply(dimnames(user_dates)[[2]], function (i) {
    
    
    if(!is.na(user_dates[as.character(id), i, "date_from"])){
      query_url <- paste0(obs_url, 
                          "user_id=", id,
                          "&d1=", user_dates[as.character(id), i, "date_from"],
                          "&d2=", user_dates[as.character(id), i, "date_to"],
                          "&only_id=TRUE")
      results_json <- fromJSON(getURL(query_url))
      results_json$total_results
    } else { NA }
  })
  
  nqs <- nqs + 2 # tally two queries
  
  tot_obs <- rbind(tot_obs, n_obs)
  
  if(nqs %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
}

# Add on to participants data
names(tot_obs) <- c("nobs_beforeCNC.2019", "nobs_afterCNC.2019")
CNC_CA.2019.participants <- cbind(CNC_CA.2019.participants, tot_obs)

# Save
#write.csv(CNC_CA.2019.participants, file = "data/CNC2019_participants.csv", row.names = FALSE)



## Veterans: download observations within 1 year buffer of 2019 CNC

# Define 50 week time period on either side of the CNC with a 1 week buffer
# This avoids the current, previous and future CNC events
beforeCNC.2019 <- as.character(c(date_from = CNC2019_start - (7*50), date_to = CNC2019_start - 7))
afterCNC.2019 <- as.character(c(date_from = CNC2019_end + 7, date_to = CNC2019_end + (7*50)))

# Define user ids to search for
user_id_string <- paste0(subset(CNC_CA.2019.participants, user.status == "veteran")$user.id,
                         collapse = ",")

use_dates <- rbind(beforeCNC.2019, afterCNC.2019)

# Find the number of observations before and after the CNC 2019 event
n_obs <- sapply(rownames(use_dates), function (i) {
  query_url <- paste0(obs_url, 
                    "user_id=", user_id_string,
                    "&taxon_id=", taxon_ids_string,
                    "&d1=", use_dates[i, "date_from"],
                    "&d2=", use_dates[i, "date_to"],
                    "&only_id=TRUE")
  results_json <- fromJSON(getURL(query_url))
  results_json$total_results
})
  
# Use a loop to extract obervations from all records across multiple pages
veteran_data.2019 <- data.frame()

for(period in rownames(use_dates)){

  # Calculate number of pages of records
  num_records <- n_obs[period]
  num_pages <- num_records %/% 30
  if(num_records %% 30 > 0) num_pages <- num_pages + 1
  
  for(i in 1:num_pages){
    query_page_url <- paste0(obs_url, 
                             "user_id=", user_id_string,
                             "&taxon_id=", taxon_ids_string,
                             "&d1=", use_dates[period, "date_from"],
                             "&d2=", use_dates[period, "date_to"],
                             "&page=",i)
    
    obs <- fromJSON(getURL(query_page_url))
    
    # Extract obs json object to a list of dataframes with only the fields needed
    obs_extract <- lapply(obs$results, extract_obs2df)
    
    # Convert list to dataframe
    obs_extract <- as.data.frame(do.call(rbind, obs_extract))
    
    # Add column identifying time period
    obs_extract$period <- period
    
    # Add to all data
    veteran_data.2019 <- rbind(veteran_data.2019, obs_extract)
    
    if(i %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
  }
}

#write.csv(veteran_data.2019, file = "data/CNC2019_obs_veteran.csv", row.names = FALSE)



## Novices: 

# Subset participants
novices <- subset(CNC_CA.2019.participants, user.status == "novice")


# Download observations for each user according to user-specific time periods
novice_data.2019 <- data.frame()

nqs <- 0 # keep track of the number of queries for rate-limiting
for(j in 1:nrow(novices)){
  
  # Determine user id
  id <- novices[j, "user.id"]
  
  # Find the number of observations 
  n_obs <- sapply(dimnames(user_dates)[[2]], function (i) {
    query_url <- paste0(obs_url, 
                        "user_id=", id,
                        "&taxon_id=", taxon_ids_string,
                        "&d1=", user_dates[as.character(id), i, "date_from"],
                        "&d2=", user_dates[as.character(id), i, "date_to"],
                        "&only_id=TRUE")
    results_json <- fromJSON(getURL(query_url))
    results_json$total_results
  })
  
  nqs <- nqs + 2 # tally 2 queries: 1 for each period
  
  if(nqs %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
  
  for(period in names(n_obs)){
    
    # Calculate number of pages of records
    num_records <- n_obs[period]
    num_pages <- num_records %/% 30
    if(num_records %% 30 > 0) num_pages <- num_pages + 1
    
    if(num_pages > 0){
    for(i in 1:num_pages){
      query_page_url <- paste0(obs_url, 
                               "user_id=", id,
                               "&taxon_id=", taxon_ids_string,
                               "&d1=", user_dates[as.character(id), period, "date_from"],
                               "&d2=", user_dates[as.character(id), period, "date_to"],
                               "&page=",i)
      
      obs <- fromJSON(getURL(query_page_url))
      
      # Extract obs json object to a list of dataframes with only the fields needed
      obs_extract <- lapply(obs$results, extract_obs2df)
      
      # Convert list to dataframe
      obs_extract <- as.data.frame(do.call(rbind, obs_extract))
      
      # Add column identifying time period
      obs_extract$period <- period
      
      # Add to all data
      novice_data.2019 <- rbind(novice_data.2019, obs_extract)
      
      nqs <- nqs + 1 # tally one query
      
      if(nqs %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
    }}
    
    nqs <- nqs + 2 # tally 2 queries
    
    if(nqs %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
  }
}

# Save data
#write.csv(novice_data.2019, file = "data/CNC2019_obs_novice.csv", row.names = FALSE)


## Newbies: Download observations made within one-year period after the 2019 CNC

# Define user ids to search for
user_id_string <- paste0(subset(CNC_CA.2019.participants, user.status == "new")$user.id,
                         collapse = ",")

# Define the time period ending 50 weeks after the CNC
date_from <- CNC2019_end + 1
date_to <- CNC2019_end + (7*50)

# Find the number of observations to be downloaded
query_url <- paste0(obs_url, 
                    "user_id=", user_id_string,
                    "&taxon_id=", taxon_ids_string,
                    "&d1=", date_from,
                    "&d2=", date_to,
                    "&only_id=TRUE")
results_json <- fromJSON(getURL(query_url))

num_records <- results_json$total_results

# Calculate number of pages of records
num_pages <- num_records %/% 30
if(num_records %% 30 > 0) num_pages <- num_pages + 1


newbie_data.2019  <- data.frame()

# Loop through pages of observations to download
for(i in 1:num_pages){
  query_page_url <- paste0(obs_url,
                           "user_id=", user_id_string,
                           "&taxon_id=", taxon_ids_string,
                           "&d1=", date_from,
                           "&d2=", date_to,
                           "&page=",i)
  
  obs <- fromJSON(getURL(query_page_url))
  
  # Extract obs json object to a list of dataframes with only the fields needed
  obs_extract <- lapply(obs$results, extract_obs2df)
  
  # Convert list to dataframe
  obs_extract <- as.data.frame(do.call(rbind, obs_extract))
  
  # Add column identifying time period
  obs_extract$period <- "afterCNC.2019"
  
  # Add to all data
  newbie_data.2019 <- rbind(newbie_data.2019, obs_extract)
  
  if(i %% 60 == 0) Sys.sleep(60) # Rate limit queries to 60 per minute
}

# Save
#write.csv(newbie_data.2019, file = "data/CNC2019_obs_new.csv", row.names = FALSE)


