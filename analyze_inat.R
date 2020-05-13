# This script analyzes information about lichens sampled during the City Nature Challenge
# Data were queried from iNaturalist on 2020-04-18 using the script query_inat.R in this R project

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
#library(glm)



# Define date ranges for pre-during-post 2019 and 2020 events
date_ranges <- read.csv("data/time_periods.csv")


# Define color scheme
period_colors <- c(before = "#575A6C", during = "#B4C540", after = "#3686C9")

# Define figure theme
use_theme <- theme_bw() + theme(axis.text = element_text(size = 10, color = "black"))


#############################################################################
### Analysis of Data set 1: Lichen observations during the 2019 and 2020 CNCs

# Load data
dat1 <- read.csv("data/CNC_CA.csv")

# Add a column identifying the event year
dat1$year <- format(as.Date(dat1$observed_on, format = "%Y-%m-%d"), "%Y")

### Table 1: Number of unique lichen observers and observations and taxa 
###          in each participating city during the 2019 and 2020 CNC events.

city_summary <- dat1 %>%
  group_by(city, year) %>%
  summarise(n_users = length(unique(user.login)),
            n_obs = n(),
            n_taxa = length(unique(taxon.id)))

arrange(city_summary, desc(n_users))

totals <- dat1 %>%
  group_by(year) %>%
  summarise(n_users = length(unique(user.login)),
            n_obs = n(),
            n_taxa = length(unique(taxon.id)))

# Save
#write.csv(city_summary, "figures/table1_lichens.csv", row.names = FALSE)



### Table: Number of unique lichen observations and taxa for each observer. 
user_summary <- dat1 %>%
  group_by(user.login, year) %>%
  summarise(n_obs = n(),
            n_taxa = length(unique(taxon.id)))

arrange(user_summary, desc(n_obs))
# mikethebirder with 95 observations of 55 taxa
# gyrrlfalcon with 61 observations of 37 taxa
# finatic with 32 observations of 23 taxa

# Save
#write.csv(user_summary, "figures/CNC_participants_summary.csv", row.names = FALSE)

### Figure 2: Histogram of lichen observations per user (highlight specific users with photos by them
max_y <- ceiling(max(table(user_summary$n_obs < 2, user_summary$year)["TRUE",])/10)*10
  
user_summary %>%
  ggplot(aes(x = n_obs)) +
  facet_grid( ~ year) + 
  geom_histogram(binwidth = 2, boundary = 0, closed = "left", fill = period_colors["during"], color = "grey10") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max_y)) +
  labs(x = "Number of lichen observations",
       y = "Number of participants") +
  use_theme +
  theme(strip.background = element_rect(fill = alpha(period_colors["during"], alpha = 0.5)))

#ggsave("figures/Fig2.svg", height = 3, width = 6)

# Calculate quantiles for specific values
user_summary %>%
#  group_by(year) %>%
  group_by() %>%
  summarise(perc1 = sum(n_obs <= 1)/n(),
            perc2 = sum(n_obs <= 2)/n(),
            perc10 = sum(n_obs < 10)/n(),
            quant50  = quantile(n_obs, probs = 0.5), 
            quant90 = quantile(n_obs, probs = 0.9),
            quant95 = quantile(n_obs, probs = 0.95))

#2019: 55% observed 1 lichen, 74% observed 2 or fewer, 90% observed 5 or fewer lichens only 5% observed 14 or more
#2020: 47% observed 1 lichen, 72% observed 2 or fewer, 90% observed 6 or fewer, 5% observed more than 10.

# Number of unique participants across both years
length(unique(user_summary$user.login)) #402

# Number of participants summed across years
nrow(user_summary) # 458

### Table of all lichen taxa observed with quality grade
taxa_summary <- dat1 %>%
  group_by(taxon.name, quality_grade) %>%
  summarise(n_obs = n()) %>%
  pivot_wider(id_cols = taxon.name, names_from = quality_grade, 
              values_from = n_obs, values_fill = list(n_obs = 0))

# Save
#write.csv(taxa_summary, "figures/taxon_summary.csv", row.names = FALSE)

### Table of all lichen taxa observed with quality grade
taxa_city_summary <- dat1 %>%
  group_by(taxon.name, city) %>%
  summarise(n_obs = n(),
            n_RG = sum(quality_grade == "research"),
            taxon.rank = unique(taxon.rank))# %>%

# Save
#write.csv(taxa_city_summary, "figures/taxon_city_summary.csv", row.names = FALSE)


# Check names against CA species checklist: TO DO
# Quick check shows a couple non-lichenized taxa to remove in the future.


###### Compare lichen observation rates to 2019 annual air quality scores

# Load 2019 air quality data from EPA Air Data site 
air <- read.csv("data/EPA_AirData/annual_aqi_by_county_2019.csv")

# Load city summary table
city_dat <- read.csv("figures/table 1.csv")
names(city_dat)[1] <- "County"

use_counties <- c("Los Angeles", "Mendocino", "Orange", "Sacramento", "San Diego", "San Francisco")

CNC_air <- air %>%
  filter(State == "California", County %in% use_counties) %>%
  select(County, Good.Days, Moderate.Days, Unhealthy.for.Sensitive.Groups.Days, Median.AQI) %>%
  left_join(city_dat)

# Figure 1
CNC_air %>%
  filter(Year == 2020) %>%
  ggplot(aes(x = Median.AQI, y = obs_pct_lichen)) +
  geom_smooth(method = "lm", color = "black", fill = paste0(period_colors["during"], "10")) +
  geom_point(pch = 21, size = 4, fill ="white") +
  use_theme +
  ylim(c(0,0.015)) +
  labs(x = "Median 2019 AQI",
       y = "Fraction of observations that are lichens")

CNC_air %>%
  filter(Year == 2020) %>%
  ggplot(aes(x = Median.AQI, y = users_pct_lichen)) +
  geom_smooth(method = "lm", color = period_colors["during"], se = FALSE) +
  geom_point(pch = 21, size = 4, fill = period_colors["during"]) +
  use_theme +
  ylim(c(0,0.06)) +
  labs(x = "Median 2019 AQI",
       y = "Fraction of participants observing lichens")

ggsave("figures/Fig1.svg", height = 3.5, width =4)

CNC_air.2020 <- subset(CNC_air, Year == 2020)
cor(CNC_air.2020$Median.AQI, CNC_air.2020$obs_pct_lichen) # r = -0.72
cor(CNC_air.2020$Median.AQI, CNC_air.2020$users_pct_lichen) # r = -.77



#############################################################################
### Analysis of Data set 2: Effect of the 2019 CNC on lichen 
### observation rates in CA. 

# Read in data
dat2 <- read.csv("data/prepostCNC_CA.csv")

# Add a column identifying the event year
dat2$year <- format(as.Date(dat2$observed_on, format = "%m/%d/%Y"), "%Y")

# Add a column identifying the time period
dat2$period <- sapply(as.Date(dat2$observed_on, format = "%m/%d/%Y"), function(x){

    date_ranges$period[(as.Date(date_ranges$date_from) <= x) & (as.Date(date_ranges$date_to) >= x)]
})

# Calculate the number of lichen observations and taxa before, during and after the CNC events
CAprepost_summary <- dat2 %>%
  group_by(year, period) %>%
  summarise(n_obs = n(),
            n_taxa = length(unique(taxon.id)),
            n_users = length(unique(user.id))
  )

# Relevel periods
CAprepost_summary$period = factor(CAprepost_summary$period, levels = c("before", "during", "after"))

### Figure 3: Comparison of (A) the number of observations of lichens 
###           and (B) number of lichen taxa observed before, during and 
###           after the CNC events within the entire state of California. 

plot_dat <- CAprepost_summary %>%
  filter(year == 2019) %>%
  pivot_longer(cols = starts_with("n"), names_to = "variable", values_to = "count") 


# Figure 3A: Observations
filter(plot_dat, variable == "n_obs") %>%
  ggplot(aes(x = period, y = count, fill = period)) +
    geom_col() + 
    scale_fill_manual(values = period_colors) + 
    ylim(c(0, 1000)) +
    use_theme + 
    theme(strip.text = element_blank(),
          legend.position = "none", 
          panel.grid.major.x = element_blank()) +
    labs(x = "", y = "Observations") 

# Save 
ggsave("figures/Fig3A.svg",  height = 1.75, width = 2, units = "in")

# Figure 3B: Taxa
filter(plot_dat, variable == "n_taxa") %>%
  ggplot(aes(x = period, y = count, fill = period)) +
  geom_col() + 
  scale_fill_manual(values = period_colors) + 
  ylim(c(0, 200)) +
  use_theme + 
  theme(strip.text = element_blank(),
        legend.position = "none", 
        panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Taxa")
    
# Save 
ggsave("figures/Fig3B.svg",  height = 1.75, width = 2, units = "in")


# Figure 3C: Observers
filter(plot_dat, variable == "n_users") %>%
  ggplot(aes(x = period, y = count, fill = period)) +
  geom_col() + 
  scale_fill_manual(values = period_colors) + 
  ylim(c(0, 300)) +
  use_theme + 
  theme(strip.text = element_blank(),
        legend.position = "none", 
        panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Observers")

# Save 
ggsave("figures/Fig3C.svg",  height = 1.75, width = 2, units = "in")


# How many total observations were made in California in 2019?
query_url <- paste0("https://api.inaturalist.org/v1/observations?", 
                    "d1=", "2019-01-01",
                    "&d2=", "2019-12-31",
                    "&place_id=", "14",
                    "&only_id=TRUE")
results_json <- fromJSON(getURL(query_url))
results_json$total_results
# 1640750

# How many total lichen observations were made in California in 2019?
# Define lichen taxa
lichen_genera_df <- read.csv("data/lichen_genera.csv")
lichen_genera_ids <- na.omit(lichen_genera_df$taxon_id)
taxon_ids_string <- paste0(lichen_genera_ids, collapse = ",")


query_url <- paste0("https://api.inaturalist.org/v1/observations?", 
                    "d1=", "2019-01-01",
                    "&d2=", "2019-12-31",
                    "&place_id=", "14",
                    "&taxon_id=", taxon_ids_string,
                    "&only_id=TRUE")
results_json <- fromJSON(getURL(query_url))
results_json$total_results
# 14606



#############################################################################
### Analysis of Data set 3: Effect of the 2019 CNCs on participant lichen observation.

CNC2019_start <- as.Date(subset(date_ranges, year == 2019 & period == "during")$date_from)


# Read in data about participants
participants <- read.csv("data/CNC2019_participants.csv")

# Number of participants who were new, novice and veteran
participants %>%
  group_by(user.status) %>%
  summarise(num = n(),
            num_before = sum(nobs_beforeCNC.2019 > 0),
            num_after = sum(nobs_afterCNC.2019 > 0),
            num_onlyCNC = sum(nobs_beforeCNC.2019 == 0 & nobs_afterCNC.2019 == 0),
            num_excluded = sum(nobs_beforeCNC.2019 == 0 | nobs_afterCNC.2019 == 0))


# Calculate the number of days in each participants time periods

participants$ndays <- NA
for(i in 1:nrow(participants)){
  
  # veteran and new = 49 weeks
  if(participants[i, "user.status"] %in% c("veteran", "new")){
    
    participants[i, "ndays"] <- 7*49
    
  }
  
  if(participants[i, "user.status"] == "novice"){
    participants[i, "ndays"] <- CNC2019_start - as.Date(participants[i, "start_date"])
    
  }
}

# Calculate total observations for each user (lichens and nonlichens)
tot_obs <- participants %>%
  select(user.login, user.status, starts_with("nobs"), ndays) %>%
  pivot_longer(cols = starts_with("nobs"), names_to = "period", values_to = "tot_obs")

# Fix period column to match dat_sum
tot_obs$period[grep("before", tot_obs$period)] <- "beforeCNC.2019"
tot_obs$period[grep("after", tot_obs$period)] <- "afterCNC.2019"


# Read in lichen observations data from before and after CNC 2019
dat.veteran <- read.csv("data/CNC2019_obs_veteran.csv")
dat.novice <- read.csv("data/CNC2019_obs_novice.csv")
dat.new <- read.csv("data/CNC2019_obs_new.csv")

dat3 <- bind_rows(dat.veteran, dat.novice, dat.new)


# Summarize observation data and join with participant data
dat_sum <- dat3 %>%
  group_by(user.login, period) %>%
  summarise(n_obs = n())


# Join dat_sum and tot_obs
dat_sum <- full_join(tot_obs, dat_sum)

# For veteran and novice users, fill in 0 if n_obs is missing, since this indicates no
# lichens were observed during these time periods
dat_sum[is.na(dat_sum$n_obs) & dat_sum$user.status %in% c("veteran", "novice"), "n_obs"] <- 0

# For new users, fill in 0 if n_obs or tot_obs are missing for period: afterCNC.2019, since this
# indicates that no observations were made even though the user account was active
dat_sum[(is.na(dat_sum$n_obs)|is.na(dat_sum$tot_obs)) & dat_sum$period == "afterCNC.2019", "n_obs"] <- 0
# NOTE: new users will have NA for tot_obs and n_obs for period: beforeCNC.2019

# Calculate the lichen observation rate (per day) 
# and the fraction of total observations that are lichens
dat_sum <- dat_sum %>%
  mutate(lichen_per_day = ifelse(tot_obs == 0, NA, n_obs/ndays),
         lichen_frac = ifelse(tot_obs == 0, NA, n_obs/tot_obs))

# Relevel period column
dat_sum$period <- factor(dat_sum$period, levels = c("beforeCNC.2019", "afterCNC.2019"),
                         labels = c("before", "after"))

# Calculate the difference in lichen observation rate for each user after - before
dat_sum_wide <- dat_sum %>%
  pivot_wider(id_cols = c("user.login", "period"), names_from = period, values_from = starts_with("lichen")) %>%
  mutate(lichen_per_day_change = lichen_per_day_after - lichen_per_day_before,
         lichen_frac_change = lichen_frac_after - lichen_frac_before) %>%
  left_join(participants[, c("user.login", "start_date", "user.status", "user.observations_count")])


# Calculate summary stats of differences
change_stats <- dat_sum_wide %>%
  filter(user.status %in% c("veteran", "novice")) %>%
  group_by(user.status) %>%
  summarise(mean_rate_change = mean(lichen_per_day_change, na.rm = TRUE),
            mean_frac_change = mean(lichen_frac_change, na.rm = TRUE))


### Figure 4A: Effects on iNat veterans & novices: Comparison of the change in 
### lichen observation rate before and after the 2019 CNC. 

max_x <- ceiling(max(dat_sum_wide$lichen_per_day_change, na.rm = TRUE)*10)/10
max_y <- 100

filter(dat_sum_wide, user.status %in% c("veteran", "novice")) %>%
  ggplot(aes(x = lichen_per_day_change)) +
    geom_histogram(binwidth = 0.1, center = 0.05) +
    facet_wrap(~ user.status) +
    geom_vline(xintercept = 0, color = period_colors["during"], size = 1.1) +
    geom_segment(data = change_stats, aes(x = mean_rate_change, xend = mean_rate_change),
                 y = 2, yend = 1, arrow = arrow(length = unit(.1, "in"), type = "closed"), color ="white") +
    use_theme +
    theme(strip.background = element_rect(fill = "white")) +
    scale_y_continuous(expand = c(0,0),
                     limits = c(0,max_y)) +
    xlim(c(-1,1)*max_x) +
    labs(x = "Change in number of lichen observations per day",
       y = "Number of participants")

# save: will add background shading later in Inkscape
ggsave("figures/Fig4A.svg", height = 2.5, width = 5)

# Paired t-test: veterans
test_dat <- dat_sum_wide[dat_sum_wide$user.status == "veteran", c("lichen_per_day_before", "lichen_per_day_after")]
test_dat <- as.matrix(na.omit(test_dat))

t.test(test_dat[,2], test_dat[,1], alternative = "two.sided", paired = TRUE)
#t = 1.02, dF = 137, p = 0.31, mean diff = 0.0092 (after - before)

# Paired t-test: novices
test_dat <- dat_sum_wide[dat_sum_wide$user.status == "novice", c("lichen_per_day_before", "lichen_per_day_after")]
test_dat <- as.matrix(na.omit(test_dat))

t.test(test_dat[,2], test_dat[,1], alternative = "two.sided", paired = TRUE)
#t = 1.60, dF = 33, p = 0.12, mean diff = -0.0091


# Shows change for each individual, but not clear
dat_sum %>%
  filter(user.status %in% c("veteran", "novice")) %>%
  ggplot(aes(x = period, y = lichen_per_day, color = period)) +
    facet_wrap(~ user.status, scales = "free") +
    geom_line(aes(group = user.login), color = "black") +
    geom_point() 

### Figure 4B: Effects on iNat veterans & novices: Comparison of fraction of observations
### that are lichens before and after the 2019 CNC. Points colored by user status.
max_x <- ceiling(max(dat_sum_wide$lichen_frac_change, na.rm = TRUE)*10)/10
max_y <- 100

dat_sum_wide %>%
  filter(user.status %in% c("veteran", "novice")) %>%
  ggplot(aes(x = lichen_frac_change)) +
  geom_histogram(binwidth = 0.1, center = 0.05) +
  facet_wrap(~ user.status) +
  geom_vline(xintercept = 0, color = period_colors["during"]) +
  geom_segment(data = change_stats, aes(x = mean_frac_change, xend = mean_frac_change),
               y = 2, yend = 1, arrow = arrow(length = unit(.1, "in"), type = "closed"), color ="white") +
  use_theme +
  theme(strip.background = element_rect(fill = "white")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,max_y)) +
  xlim(c(-1,1)*max_x) +
  labs(x = "Change in the fraction of observations that are lichens",
       y = "Number of participants")

# save: will add background shading later in Inkscape
ggsave("figures/Fig4B.svg", height = 2.5, width = 5)


# Paired t-test: veterans
test_dat <- dat_sum_wide[dat_sum_wide$user.status == "veteran", c("lichen_frac_before", "lichen_frac_after")]
test_dat <- as.matrix(na.omit(test_dat))

t.test(test_dat[,2], test_dat[,1], alternative = "two.sided", paired = TRUE)
#t = 1.03, dF = 137, p = 0.31, mean diff = -0.0028 (after - before)

# Paired t-test: novices
test_dat <- dat_sum_wide[dat_sum_wide$user.status == "novice", c("lichen_frac_before", "lichen_frac_after")]
test_dat <- as.matrix(na.omit(test_dat))

t.test(test_dat[,2], test_dat[,1], alternative = "two.sided", paired = TRUE)
#t = 0.7, dF = 33, p = 0.48, mean diff = 0.021


# Who was the one person who changed?
subset(dat_sum_wide, lichen_frac_change > .5)  # user.login = sjiang96

subset(dat_sum, user.login == "sjiang96")
# iNat user for 65 days before the 2019 CNC
# had made 74 total obs before, none of which were lichen and made one after, which was a lichen

subset(dat3, user.login == "sjiang96") # Flavoparmelia caperata observed after CNC 2019
subset(dat1, user.login == "sjiang96") # Flavopunctelia flaventior obsered during CNC 2019


### Effects on iNat newbies: Calculate the fraction who continued to observe lichens 
### over the next 50 weeks. 

new_stats <- subset(dat_sum, user.status == "new"& period == "after")

new_stats %>%
  summarize(iNat = sum(tot_obs > 0),
            lichen = sum(n_obs > 0))
nrow(new_stats)

