# This script analyzes information about lichens sampled during the City Nature Challenge
# Data were queried from iNaturalist in Jan-Feb 2023 using the script query_inat.R in this R project

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan) # rarefaction



# Define date ranges for pre-during-post 2018:2022
date_ranges <- read.csv("data/time_periods.csv")


# Define color scheme
period_colors <- c(before = "#575A6C", during = "#B4C540", after = "#3686C9")

# Define figure theme
use_theme <- theme_bw() + theme(axis.text = element_text(size = 10, color = "black"))

# Load city list that includes participation stats from official CNC page
cities <- read.csv("data/CNC_cities_minPeople100_withLocality.csv", fileEncoding = "UTF-8")

# Load lichen genera ids from iNat
lichen_genera <- read.csv("data/lichen_genera.csv") # created in query_inat.R script
lookup_genus_id <- unique(lichen_genera[, c("Genus", "id")])

# Load full taxonomy from iNaturalist downloaded on 2023-02-01 and 
# generate list of lichen-forming fungal taxa in iNaturalist
#iNat_taxa <- read.csv("data/inaturalist-taxonomy.dwca_20230201/taxa.csv")
#iNat_lichens <- subset(iNat_taxa, (genus %in% lichen_genera$Genus))
#write.csv(iNat_lichens, "data/lichen_taxa.csv", row.names = FALSE)
iNat_lichens <- read.csv("data/lichen_taxa.csv")

# Define function that looks up the genus of a lichen taxon from its iNat ID
# Requires that iNat_lichens are already present in the environment
# Works regardless of whether x is a character or numeric, but doesn't work on vectors
find_genus <- function(x, lookup){
  genus_name <- subset(iNat_lichens, id == x)$genus
  genus_id <- subset(lookup, Genus == genus_name)$id
  genus_id
}




### ANALYSIS QUESTIONS:
# PART 1
# Does the observation rate of lichens increase during CNC events?
#   If so, is the increased rate proportional to the increase in overall observations or the number of observers?
#   Does the rate of lichen observation remain elevated after the CNC events?
# Do these rates differ by country or city?

# Does the diversity of lichens observed increase during CNC events?
#   If so, is this rate greater than would be expected simply from increased sampling?

# Does the number of research grade observations increase during CNC events?

# PART 2
# Who observes lichens during CNC events: new users, novices, or veteran users? Have users observed lichens previously?
# Do users who observed lichens during a CNC continue to do so after the event? Are these primarily new, novice or veteran users?



#########################################################################################
### PART 1: Lichen observations before, during and after the CNC events

# Load data
dat1 <- read.csv("data/CNC_lichen_obs.csv")
dat2 <- read.csv("data/CNC_obs_count.csv")

# Set levels for period
dat1$Period <- factor(dat1$Period, levels = c("before", "during", "after"))
dat2$Period <- factor(dat2$Period, levels = c("before", "during", "after"))


### Tables: Number of unique lichen observers and observations and taxa 
###          in each participating city before, during and after each event

city_summary <- dat1 %>%
  group_by(City, Year, Period) %>%
  summarise(n_users = length(unique(user.login)),
            n_obs = n(),
            n_obs_RG = sum(quality_grade == "research"),
            taxa = paste(unique(taxon.id), collapse = " "))

taxa_list <- lapply(strsplit(city_summary$taxa, " "), as.numeric)

genera_list <- lapply(taxa_list, function(x){
  sapply(x, function(x){
    find_genus(x, lookup = lookup_genus_id)
  })
})

city_summary$n_taxa <- sapply(1:length(taxa_list), function(i){
  taxa <- taxa_list[[i]]
  genera <- genera_list[[i]]
  genus_taxa <- taxa[taxa %in% genera] # taxa that are actually genera
  count_genera <- table(genera)[as.character(genus_taxa)] # how many taxa are in these genera?
  drop_taxa <- sum(count_genera > 1) # number of taxa to drop because they are already represent by other taxa in the same genus
  length(taxa) - drop_taxa # count the number of taxa left after dropping these genera
})

arrange(city_summary, desc(n_taxa))

# Append column with total number of observations
city_summary <- full_join(city_summary, dat2) %>%
  rename(n_obs_all = Observations.All)

# Add in 0 for lichen observations that are missing
city_summary[is.na(city_summary$n_obs), c('n_obs', 'n_taxa', 'n_users')] <- 0


city_summary <- select(city_summary, 
                       City, Year, Period, n_obs, n_obs_RG, n_taxa, n_users, n_obs_all, taxa)

# Save
#write.csv(city_summary, "figures/table_city_summary.csv", row.names = FALSE)


# Read in summary table
city_summary <- read.csv("figures/table_city_summary.csv")

# Set levels of Period
city_summary$Period <- factor(city_summary$Period, levels = c("before", "during", "after"))

# Define distinct CNC events that had lichen observations and merge with stats during each event
events <- filter(city_summary, Period == "during") %>%
  filter(n_obs > 0) %>%
  select(-taxa, -Period) %>%
  mutate(EventID = paste(City, Year, sep = "_")) %>%
  left_join(select(cities, City, Year, People, PlaceID))

# Add eventID to dat1
dat1$EventID <- with(dat1, paste(City, Year, sep = "_"))

# Summarize by year lumping cities together
# Note: this includes events with fewer than 100 participants
year_summary <- dat1 %>%
  filter(EventID %in% events$EventID) %>%
  group_by(Year, Period) %>%
  summarise(n_cities = length(unique(City)),
            n_users = length(unique(user.login)),
            n_obs = n(),
            n_obs_RG = sum(quality_grade == "research"),
            taxa = paste(unique(taxon.id), collapse = " "))

# Count the number of taxa after excluding genus-level taxa for which there are species-level taxa present
taxa_list <- lapply(strsplit(year_summary$taxa, " "), as.numeric)

genera_list <- lapply(taxa_list, function(x){
  sapply(x, function(x){
    find_genus(x, lookup = lookup_genus_id)
  })
})

year_summary$n_taxa <- sapply(1:length(taxa_list), function(i){
  taxa <- taxa_list[[i]]
  genera <- genera_list[[i]]
  genus_taxa <- taxa[taxa %in% genera] # taxa that are actually genera
  count_genera <- table(genera)[as.character(genus_taxa)] # how many taxa are in these genera?
  drop_taxa <- sum(count_genera > 1) # number of taxa to drop because they are already represent by other taxa in the same genus
  length(taxa) - drop_taxa # count the number of taxa left after dropping these genera
})

# Summarize total observations of all taxa before, during and after CNC events
dat2$EventID <- with(dat2, paste(City, Year, sep = "_"))
year_summary_all <- dat2 %>%
  filter(EventID %in% events$EventID) %>%
  group_by(Year, Period) %>%
  summarise(n_obs_all = sum(Observations.All))

# Add to summary table
year_summary <- full_join(year_summary, year_summary_all)

year_summary <- select(year_summary, 
                       Year, Period, n_cities, n_obs, n_obs_RG, n_taxa, n_users, n_obs_all, taxa)

arrange(year_summary, desc(n_obs))

# Save
#write.csv(year_summary, "figures/table_year_summary.csv", row.names = FALSE)

# Read in summary table
year_summary <- read.csv("figures/table_year_summary.csv")
year_summary$Period <- factor(year_summary$Period, levels = c("before", "during", "after"))

# Overall stats on all events analyzed
overall_summary <- year_summary %>%
  group_by(Period) %>%
  summarise(n_cities = sum(n_cities), # number of cities observing lichens
            n_obs = sum(n_obs),
            n_obs_RG = sum(n_obs_RG),
            n_obs_all = sum(n_obs_all),
            avg_taxa = mean(n_taxa),
            avg_users = mean(n_users))

# # A tibble: 3 × 7
#   Period n_cities n_obs n_obs_RG n_obs_all avg_taxa avg_users
#   <fct>     <int> <int>    <int>     <int>    <dbl>     <dbl>
# 1 before      316  3172      842    559907     146.      276.
# 2 during      509 30649     9707   4433131     449      1879 
# 3 after       310  2546      681    627816     132.      235 


### Does the observation rate of lichens increase during CNC events?
##   If so, is the increased rate proportional to the increase in overall observations or the number of observers?
##   Does the rate of lichen observation remain elevated after the CNC events?


## Compare lichen observation rates across all years

# Lichen observation rates
with(overall_summary, n_obs/n_obs_all)
with(overall_summary, n_obs_RG/n_obs)

# Comparison to before and after
use_before <- subset(overall_summary, Period == "before")
use_during <- subset(overall_summary, Period == "during")
use_after <- subset(overall_summary, Period == "after")

# Before compared to during
(use_during$n_obs - use_before$n_obs)/use_before$n_obs # percent increase of lichen obs = 866%
(use_during$n_obs_RG - use_before$n_obs_RG)/use_before$n_obs_RG # percent increase of research grade lichen obs = 1053%

(with(use_during, n_obs/n_obs_all) - with(use_before, n_obs/n_obs_all)) / with(use_before, n_obs/n_obs_all)
chisq.test(rbind(use_before[, c("n_obs", "n_obs_all")],
                 use_during[, c("n_obs", "n_obs_all")]))
(with(use_during, n_obs_RG/n_obs) - with(use_before, n_obs_RG/n_obs)) / with(use_before, n_obs_RG/n_obs)
chisq.test(rbind(use_before[, c("n_obs", "n_obs_RG")],
           use_during[, c("n_obs", "n_obs_RG")]))

# Before compared to after
(use_during$n_obs - use_after$n_obs)/use_after$n_obs # percent increase of lichen obs = 1098%
(use_during$n_obs_RG - use_after$n_obs_RG)/use_after$n_obs_RG # percent increase of lichen obs = 853%

(with(use_after, n_obs/n_obs_all) - with(use_before, n_obs/n_obs_all)) / with(use_before, n_obs/n_obs_all)
chisq.test(rbind(use_before[, c("n_obs", "n_obs_all")],
                 use_after[, c("n_obs", "n_obs_all")]))
(with(use_after, n_obs_RG/n_obs) - with(use_before, n_obs_RG/n_obs)) / with(use_before, n_obs_RG/n_obs)
chisq.test(rbind(use_before[, c("n_obs", "n_obs_RG")],
                 use_after[, c("n_obs", "n_obs_RG")]))

## Compare lichen observations within each year

# Calculate lichen observation rate relative to total number of observations
year_summary$obs_rate <- with(year_summary, n_obs/n_obs_all)

# Calculate lichen observation rate relative to number of people observing lichens
year_summary$obs_rate_pp <- with(year_summary, n_obs/n_users)

# Table 1: Calculate percent and absolute increases during relative to before events
table1 <- year_summary %>%
  select(Year, Period, n_obs, n_obs_RG, n_taxa, n_users) %>%
  pivot_wider(names_from = Period, 
              values_from = starts_with("n_"),
              names_sep = ".") %>%
  transmute(Year,
            n_obs.pct = (n_obs.during - n_obs.before)/n_obs.before,
            n_obs_RG.pct = (n_obs_RG.during - n_obs_RG.before)/n_obs_RG.before,
            n_taxa.pct = (n_taxa.during - n_taxa.before)/n_taxa.before,
            n_taxa.abs = n_taxa.during - n_taxa.before,
            n_users.pct = (n_users.during - n_users.before)/n_users.before,
            n_users.abs = n_users.during - n_users.before) %>%
  left_join(filter(year_summary, Period == "during")) %>%
  select(Year, n_cities, n_obs, n_obs.pct, n_obs_RG, n_obs_RG.pct, n_users, n_users.pct, n_taxa, n_taxa.pct)

write.csv(table1, "figures/table1_annual_summary.csv", row.names = FALSE)

# # A tibble: 5 × 10
# Year n_cities n_obs n_obs.pct n_obs_RG n_obs_RG.pct n_users n_users.pct n_taxa n_taxa.pct
# <int>    <int> <int>     <dbl>    <int>        <dbl>   <int>       <dbl>  <int>      <dbl>
# 1  2018       40  3159     23.7      1377        19.6      895       13.9     391      6.11 
# 2  2019       68  5986     21.9      2244        38.4     1738       11.1     593      4.81 
# 3  2020      104  5241      7.44     1594         7.76    1709        5.86    315      1.5  
# 4  2021      138  6958      7.88     2024        10.1     2213        4.76    434      1.41 
# 5  2022      159  9305      5.75     2468         5.99    2840        4.25    512      0.932



# Chi-squared test comparing before to during lichen observation rates in each year
chisq_tests_byYear <- data.frame()

for(year in 2018:2022){
  use_before <- filter(year_summary, Year == year, Period =="before")[, c("n_obs", "n_obs_all", "obs_rate")]
  use_during <- filter(year_summary, Year == year, Period =="during")[, c("n_obs", "n_obs_all", "obs_rate")]
  use_after <- filter(year_summary, Year == year, Period =="after")[, c("n_obs", "n_obs_all", "obs_rate")]
  
  
  chisq_before <- chisq.test(rbind(use_before[, c("n_obs", "n_obs_all")],
                                   use_during[, c("n_obs", "n_obs_all")]))
  chisq_after <- chisq.test(rbind(use_before[, c("n_obs", "n_obs_all")],
                                  use_after[, c("n_obs", "n_obs_all")]))
  
  chisq_tests_byYear <- rbind(chisq_tests_byYear,
                              data.frame(Year = year, 
                                         Comparison = c("during", "after"), 
                                         Difference = c(use_during$obs_rate - use_before$obs_rate,
                                                        use_after$obs_rate - use_before$obs_rate),
                                         Chisq = as.numeric(c(chisq_before$statistic, chisq_after$statistic)),
                                         P = c(chisq_before$p.value, chisq_after$p.value))
  )
  
}

arrange(chisq_tests_byYear, Comparison)
# Year Comparison    Difference       Chisq            P
# 1  2018      after -0.0002043101   0.1241757 7.245490e-01
# 2  2019      after -0.0004574478   1.4186930 2.336187e-01
# 3  2020      after -0.0022327152  64.1734791 1.139321e-15
# 4  2021      after -0.0008765397  16.8853753 3.970634e-05
# 5  2022      after -0.0025269705 104.2176093 1.812723e-24
# 6  2018     during  0.0033829759  42.5369972 6.935781e-11
# 7  2019     during  0.0025843577  54.4289057 1.611746e-13
# 8  2020     during  0.0013935079  25.7603401 3.865505e-07
# 9  2021     during  0.0020590472 100.9992182 9.201976e-24
# 10 2022     during -0.0005213260   6.3827492 1.152348e-02


# Chi-squared test comparing before to during research grade observation rates in each year
chisq_tests_RG_byYear <- data.frame()

for(year in 2018:2022){
  use_before <- filter(year_summary, Year == year, Period =="before")[, c("n_obs", "n_obs_RG")]
  use_during <- filter(year_summary, Year == year, Period =="during")[, c("n_obs", "n_obs_RG")]
  use_after <- filter(year_summary, Year == year, Period =="after")[, c("n_obs", "n_obs_RG")]
  
  
  
  chisq_before <- chisq.test(rbind(use_before[, c("n_obs_RG", "n_obs")],
                                   use_during[, c("n_obs_RG", "n_obs")]))
  chisq_after <- chisq.test(rbind(use_before[, c("n_obs_RG", "n_obs")],
                                  use_after[, c("n_obs_RG", "n_obs")]))
  
  RG_rates <- c(before = use_before$n_obs_RG/use_before$n_obs,
                during = use_during$n_obs_RG/use_during$n_obs,
                after = use_after$n_obs_RG/use_after$n_obs)
  
  chisq_tests_RG_byYear <- rbind(chisq_tests_RG_byYear,
                                 data.frame(Year = year, 
                                            Comparison = c("during", "after"),
                                            RG_rate = RG_rates[c("during", "after")],
                                            Difference = RG_rates[c("during", "after")] - RG_rates["before"],
                                            Chisq = as.numeric(c(chisq_before$statistic, chisq_after$statistic)),
                                            P = c(chisq_before$p.value, chisq_after$p.value))
  )
  
}

arrange(chisq_tests_RG_byYear, Comparison)
# Year Comparison   RG_rate   Difference      Chisq            P
# after   2018      after 0.2531646 -0.270272943  9.2359539 0.0023730875
# after1  2019      after 0.2666667  0.048275862  0.7812071 0.3767722249
# after2  2020      after 0.2823276 -0.010748098  0.0495449 0.8238570848
# after3  2021      after 0.2056632 -0.027755178  0.9140393 0.3390449275
# after4  2022      after 0.3040474  0.047879024  3.6020495 0.0577083856
# during  2018     during 0.4358974 -0.087540064  1.2295183 0.2675007441
# during1 2019     during 0.3748747  0.156483903 13.1106544 0.0002936211
# during2 2020     during 0.3041404  0.011064747  0.1385351 0.7097417206
# during3 2021     during 0.2908882  0.057469819  6.3813441 0.0115326059
# during4 2022     during 0.2652337  0.009065385  0.2635767 0.6076734941


# Chi-squared test comparing before to during  observation rates per person in each year
chisq_tests_pp_byYear <- data.frame()

for(year in 2018:2022){
  use_before <- filter(year_summary, Year == year, Period =="before")[, c("n_obs", "n_users")]
  use_during <- filter(year_summary, Year == year, Period =="during")[, c("n_obs", "n_users")]
  use_after <- filter(year_summary, Year == year, Period =="after")[, c("n_obs", "n_users")]
  
  
  
  chisq_before <- chisq.test(rbind(use_before[, c("n_obs", "n_users")],
                                   use_during[, c("n_obs", "n_users")]))
  chisq_after <- chisq.test(rbind(use_before[, c("n_obs", "n_users")],
                                  use_after[, c("n_obs", "n_users")]))
  
  pp_rates <- c(before = use_before$n_obs/use_before$n_users,
                during = use_during$n_obs/use_during$n_users,
                after = use_after$n_obs/use_after$n_users)
  
  chisq_tests_pp_byYear <- rbind(chisq_tests_pp_byYear,
                                 data.frame(Year = year, 
                                            Comparison = c("during", "after"),
                                            pp_rate = pp_rates[c("during", "after")],
                                            Difference = pp_rates[c("during", "after")] - pp_rates["before"],
                                            Chisq = as.numeric(c(chisq_before$statistic, chisq_after$statistic)),
                                            P = c(chisq_before$p.value, chisq_after$p.value))
  )
  
}

arrange(chisq_tests_pp_byYear, Comparison)
# Year Comparison  pp_rate  Difference        Chisq            P
# after   2018      after 2.164384  0.03105023 2.854517e-30 1.000000e+00
# after1  2019      after 2.162162  0.34966216 1.130379e+00 2.876946e-01
# after2  2020      after 1.917355 -0.57662053 5.553992e+00 1.843859e-02
# after3  2021      after 1.848485 -0.19318182 1.118970e+00 2.901404e-01
# after4  2022      after 2.624352  0.07721740 1.166867e-01 7.326562e-01
# during  2018     during 3.529609  1.39627561 9.412227e+00 2.155432e-03
# during1 2019     during 3.444189  1.63168872 3.612929e+01 1.846506e-09
# during2 2020     during 3.066706  0.57272977 6.480056e+00 1.090915e-02
# during3 2021     during 3.144148  1.10248155 4.167311e+01 1.078829e-10
# during4 2022     during 3.276408  0.72927352 2.071294e+01 5.335423e-06


## Chi-squared test comparing before to during lichen observation rates in each event (city-year)

# Calculate lichen observation rate
city_summary$obs_rate <- with(city_summary, n_obs/n_obs_all)

# Calculate lichen observation rate
city_summary$obs_rate_pp <- with(city_summary, n_obs/n_users)

# TODO: Compare observation rate per person before-during and before-after 
#       for events that had users observing lichens in all three time periods
#       Actually- this should also be done in Part 2 since it doesn't make sense to calculated the avg rate for each city and then compare

# Calculate observation rate difference for all events conduct chisquared tests for 
# significant differences in rate between before-during and before-after for events with enough observations
chisq_tests_byEvent <- data.frame()

for(i in 1:nrow(events)){
  this_city <- events[i, "City"]
  year <- events[i, "Year"]
  
  use_before <- filter(city_summary, City == this_city, Year == year, Period =="before")[, c("n_obs", "n_obs_all", "n_obs_RG", "obs_rate")]
  use_during <- filter(city_summary, City == this_city, Year == year, Period =="during")[, c("n_obs", "n_obs_all", "n_obs_RG", "obs_rate")]
  use_after <- filter(city_summary, City == this_city, Year == year, Period =="after")[, c("n_obs", "n_obs_all", "n_obs_RG", "obs_rate")]
  
  # if there are enough observations, do a chi-squared test
  during_obs <- rbind(use_before, use_during)[c("n_obs", "n_obs_all")]
  during_exp <- colSums(during_obs) %*% t(rowSums(during_obs))/sum(during_obs)
  after_obs <- rbind(use_before, use_after)[c("n_obs", "n_obs_all")]
  after_exp <- colSums(after_obs) %*% t(rowSums(after_obs))/sum(after_obs)
  
  Chisq <- c(NA, NA)
  P <- c(NA, NA)
  if(all(during_exp > 5)){
    during_chisq <- chisq.test(during_obs)
    Chisq[1] <- during_chisq$statistic
    P[1] <- during_chisq$p.value
  }
  if(all(after_exp > 5)){
    after_chisq <- chisq.test(after_obs)
    Chisq[1] <- after_chisq$statistic
    P[1] <- after_chisq$p.value
  }
  chisq_tests_byEvent <- rbind(chisq_tests_byEvent,
                               data.frame(City = this_city,
                                          Year = year, 
                                          Comparison = c("during", "after"),
                                          rate_before = rep(use_before$obs_rate, 2),
                                          rate = c(use_during$obs_rate, use_after$obs_rate),
                                          rate_difference = c(use_during$obs_rate - use_before$obs_rate,
                                                              use_after$obs_rate - use_before$obs_rate),
                                          Chisq = Chisq,
                                          P = P)
  )
}

# Save tables
#write.csv(chisq_tests_byEvent, file = "figures/table_lichen_obs_rate_diffs_byEvent.csv", row.names = FALSE)
#write.csv(chisq_tests_byYear, file = "figures/table_lichen_obs_rate_diffs_byYear.csv", row.names = FALSE)
#write.csv(chisq_tests_RG_byYear, file = "figures/table_lichen_RG_obs_rate_diffs_byYear.csv", row.names = FALSE)
#write.csv(chisq_tests_pp_byYear, file = "figures/table_lichen_pp_obs_rate_diffs_byYear.csv", row.names = FALSE)





### FIGURES ###

# Figure: lichen observations before, during and after CNC events in different years
year_summary %>%
  ggplot(aes(x = Period, y = n_obs)) +
    facet_wrap(~Year) + 
    #geom_col(aes(y = n_obs_all), color = "grey50", fill = "white") +
    geom_col(aes(fill = Period)) +
    geom_text(aes(label = paste(format(n_obs/n_obs_all*100, digits = 2), "%")), vjust = -0.5) +
    # TODO: Add in number of cities analyzed in each panel
    scale_fill_manual(values = period_colors) + 
    ylim(c(0, 10000)) +
    use_theme + 
    theme(legend.position = "none", 
        panel.grid.major.x = element_blank()) +
    labs(x = "", y = "Observations")


# Save 
ggsave("figures/figure_lichen_obs_byYear.svg",  height = 5, width = 7, units = "in")

year_summary %>%
  ggplot(aes(x = Year, y = n_obs, color = Period)) +
  geom_line(size = 2) +
  geom_text(aes(label = paste(format(n_obs/n_obs_all*100, digits = 2), "%")), vjust = -0.5) +
  # TODO: Add in number of cities analyzed in each panel
  scale_color_manual(values = period_colors) + 
  ylim(c(0, 10000)) +
  use_theme + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank()) +
  labs(x = "Year", y = "Observations")

# Figure: research-grade lichen observations before, during and after CNC events in different years
year_summary %>%
  ggplot(aes(x = Period, y = n_obs_RG)) +
  facet_wrap(~Year) + 
  #geom_col(aes(y = n_obs_all), color = "grey50", fill = "white") +
  geom_col(aes(fill = Period)) +
  geom_text(aes(label = paste(format(n_obs_RG/n_obs*100, digits = 2), "%")), vjust = -0.5) +
  # TODO: Add in number of cities analyzed in each panel
  scale_fill_manual(values = period_colors) + 
  ylim(c(0, 3000)) +
  use_theme + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Research-grade Observations")


# Save 
ggsave("figures/figure_lichen_obs_RG_byYear.svg",  height = 5, width = 7, units = "in")


# Figure: lichen observation rate per person before, during and after CNC events in different years
year_summary %>%
  ggplot(aes(x = Period, y = obs_rate_pp)) +
  facet_wrap(~Year) + 
  geom_col(aes(fill = Period)) +
  geom_text(aes(label = n_obs), vjust = -0.5) +
  # TODO: Add in number of cities analyzed in each panel
  scale_fill_manual(values = period_colors) + 
  ylim(c(0, 4)) +
  use_theme + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Observations per person")


# Compare change in observation rate during CNC events to change in rate after CNC events
chisq_tests_byEvent %>%
  select(City, Year, Comparison, rate_difference) %>%
  pivot_wider(names_from = Comparison, values_from = rate_difference) %>%
  ggplot(aes(x = during, y = after)) +
    geom_point(color = "grey50") +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    geom_abline(aes(slope = 1, intercept = 0)) +
    coord_equal()


# Compare observation rate during to observation rate after
chisq_tests_byEvent %>%
    select(City, Year, Comparison, rate) %>%
    pivot_wider(names_from = Comparison, values_from = rate) %>%
    ggplot(aes(x = during, y = after)) +
      geom_point(color = "grey50") +
      geom_hline(aes(yintercept = 0)) +
      geom_vline(aes(xintercept = 0)) +
      geom_abline(aes(slope = 1, intercept = 0)) +
      coord_equal()

# Examine which events had significant changes in obs rate from before the event
chisq_tests_byEvent %>%
  ggplot(aes(x = rate_before, y = rate_difference)) +
    facet_wrap( ~ Comparison) +
    geom_point(color = "grey50") +
    geom_point(data = filter(chisq_tests_byEvent, !is.na(P)), aes(color = P < 0.05)) + 
    geom_hline(yintercept = 0)




## Compare number of taxa found in the three periods


# Figure: lichen taxa before, during and after CNC events in different years
year_summary %>%
  ggplot(aes(x = Period, y = n_taxa)) +
  facet_wrap(~Year) + 
  #geom_col(aes(y = n_obs_all), color = "grey50", fill = "white") +
  geom_col(aes(fill = Period)) +
  scale_fill_manual(values = period_colors) + 
  ylim(c(0, 600)) +
  use_theme + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Num. taxa")

# Save 
ggsave("figures/figure_lichen_taxa_byYear.svg",  height = 1.75, width = 2, units = "in")



## Uses rarefaction on dat1 to compare taxonomic richness before and during events

# Make community data array: [year, period, taxa]
comm_dat <- with(dat1, table(Year, Period, taxon.id))

rarecurves <- data.frame()

for(year in 2018:2022){
  
  this_curve <- rarecurve(comm_dat[as.character(year),,])
  this_dat <- data.frame(Year = year,
                         n_taxa = unlist(this_curve),
                         N = c(attr(this_curve[[1]], "Subsample"),
                               attr(this_curve[[2]], "Subsample"),
                               attr(this_curve[[3]], "Subsample")),
                         Period = rep(dimnames(comm_dat)$Period,
                                      times = unlist(lapply(this_curve, length)))
  )
  
  rarecurves <- rbind(rarecurves, this_dat)
  
}


# Make data to plot that filters to the min number of observations
plot_dat <- data.frame()
for(year in 2018:2022){
  minN <- min(subset(year_summary, Year == year)$n_obs)
  plot_dat <- rbind(plot_dat,
                    filter(rarecurves, Year == year, N <= minN)
  )
}

# Plot rarefaction curves
plot_dat %>%
  ggplot(aes(x = N, y = n_taxa)) +
    geom_line(aes(color = Period)) +
    facet_wrap( ~ Year)





####### WORKING HERE #########


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









#########################################################################################
### PART 2: Lichen observers before, during and after the CNC events

# Load data
dat1 <- read.csv("data/CNC_lichen_obs.csv")



### Table: Number of unique lichen observations and taxa for each observer in each year. 
user_summary <- dat1 %>%
  group_by(user.login, City, Year, Period) %>%
  summarise(n_obs = n(),
            taxa = paste(unique(taxon.id), collapse = " "))

# Count the number of taxa after excluding genus-level taxa for which there are species-level taxa present
taxa_list <- lapply(strsplit(user_summary$taxa, " "), as.numeric)

genera_list <- lapply(taxa_list, function(x){
  sapply(x, function(x){
    find_genus(x, lookup = lookup_genus_id)
  })
})

user_summary$n_taxa <- sapply(1:length(taxa_list), function(i){
  taxa <- taxa_list[[i]]
  genera <- genera_list[[i]]
  genus_taxa <- taxa[taxa %in% genera] # taxa that are actually genera
  count_genera <- table(genera)[as.character(genus_taxa)] # how many taxa are in these genera?
  drop_taxa <- sum(count_genera > 1) # number of taxa to drop because they are already represent by other taxa in the same genus
  length(taxa) - drop_taxa # count the number of taxa left after dropping these genera
})

user_summary <- select(user_summary, 
                       user.login, Year, City, Period, n_obs, n_taxa, taxa)


arrange(user_summary, desc(n_obs))


# Save
#write.csv(user_summary, "figures/table_user_summary.csv", row.names = FALSE)


#### WORKING HERE #####

### Figure 2: Histogram of lichen observations per user (highlight specific users with photos by them
max_y <- ceiling(max(table(user_summary$n_obs < 2, user_summary$year)["TRUE",])/10)*10
  
user_summary %>%
  ggplot(aes(x = n_obs)) +
  facet_grid( ~ year) + 
  geom_histogram(binwidth = 2, boundary = 0, closed = "left", fill = period_colors["during"], color = "grey10") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max_y)) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Number of lichen observations",
       y = "Number of participants") +
  use_theme +
  theme(strip.background = element_rect(fill = alpha(period_colors["during"], alpha = 0.5)))

#ggsave("figures/Fig2.svg", height = 3, width = 6)

# Calculate quantiles for specific values
user_summary %>%
  group_by(year) %>%
 # group_by() %>%
  summarise(total = n(),
            perc1 = sum(n_obs <= 1)/n(),
            perc2 = sum(n_obs <= 2)/n(),
            perc10 = sum(n_obs < 10)/n(),
            quant50  = quantile(n_obs, probs = 0.5), 
            quant90 = quantile(n_obs, probs = 0.9),
            quant95 = quantile(n_obs, probs = 0.95))

#2019: 55% observed 1 lichen, 74% observed 2 or fewer, 90% observed 5 or fewer lichens only 5% observed 14 or more
#2020: 47% observed 1 lichen, 72% observed 2 or fewer, 90% observed 6 or fewer, 5% observed more than 10.

# Number of unique participants across both years
length(unique(user_summary$user.login)) #420

# Number of participants summed across years
nrow(user_summary) # 477

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


#














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

