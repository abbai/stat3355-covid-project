library(ggplot2)
library(janitor)
library(dplyr)
library(stringr)

### READ IN DATASETS ... ###

# CASES
cases <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/us_timeseries_confirmed.txt", 
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)

# DEATHS
deaths <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/us_timeseries_deaths.txt", 
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)

# MORTALITY RATE DF
mortality_rate_df <- cases[c(1:11, length(cases))]
mortality_rate_df$cases <- cases[c(length(cases))]
mortality_rate_df$deaths <- deaths[c(length(cases))]
mortality_rate_df$ratio <- mortality_rate_df$deaths / mortality_rate_df$cases
mortality_rate_df$ratio <- as.vector(mortality_rate_df$ratio$X10.15.22)

# RACE
race <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/race_data.csv",
                 fill = TRUE)

# EMPLOYMENT
employment <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/laucnty21 (1).csv",
                 fill = TRUE)
employment <- row_to_names(employment, 4, remove_rows_above = TRUE)
employment <- clean_names(employment)
employment <- employment[-1,]

# UID reference
uidref <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/uidlookup.txt",
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)
uidref <- uidref[which(uidref$iso3 == "USA"),]
uidref <- uidref[-c(2:177),]

# POVERTY 
pov <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/poverty_data.csv", header = FALSE)
pov <- row_to_names(pov, 4, remove_rows_above = TRUE)
pov <- clean_names(pov)
pov <- pov[-1,]

### CLEANING ... ####

# MORTALITY RATE DF
# .05 is cdc cutoff for mortality rates !!! anything above it is inaccurate
mortality_rate_df <- mortality_rate_df[which(mortality_rate_df$ratio < .05),]
# do not include entries with 0 deaths; throws off our stats, likely inaccurate
mortality_rate_df <- mortality_rate_df[which(mortality_rate_df$deaths > 0),]
# do not include entries with missing info
mortality_rate_df <- na.omit(mortality_rate_df)
# only keep the states
mortality_rate_df[mortality_rate_df == ""] <- NA
mortality_rate_df <- filter(mortality_rate_df, is.na(Admin2) == FALSE)
# Keep only fips, mortality ratio columns
mortality_rate_df <- select(mortality_rate_df, FIPS, ratio)
mortality_rate_df$FIPS <- as.character(mortality_rate_df$FIPS)

# POVERTY
# keep only counties (not states in general)
povtemp <- pov$name == state.abb[match(pov$name, state.name)]
pov <- pov[which(is.na(povtemp)),]
# simplify county names... NOTE: didnt really need to do this lol
pov$UID <- paste("840", pov$state_fips_code, pov$county_fips_code)
pov$UID <- gsub(" ", "", pov$UID)
pov$UID <- as.integer(pov$UID)
temppov2 <- left_join(pov, uidref)
pov$name <- temppov2$Admin2
# fix fips to include state code
pov$county_fips_code <- paste(pov$state_fips_code, pov$county_fips_code)
pov$county_fips_code <- gsub(" ", "", pov$county_fips_code)
pov$county_fips_code <- str_remove(pov$county_fips_code, "^0+")
# keep only fips, poverty percentage, median household income
pov <- select(pov, county_fips_code, poverty_percent_all_ages, median_household_income)
pov <- rename(pov, FIPS = county_fips_code)

# EMPLOYMENT
# fix fips to include state code
employment$code <- paste(employment$code_2, employment$code_3)
employment$code <- gsub(" ", "", employment$code)
employment$code <- str_remove(employment$code, "^0+")
# keep only fips, unemployment percent
employment <- select(employment, code, percent)
employment <- rename(employment, FIPS = code)
employment <- rename(employment, unemp_percent = percent)

# JOIN MAIN DATA FRAME INFO
test_frame <- inner_join(mortality_rate_df, pov)
maindf <- inner_join(test_frame, employment)
maindf$poverty_percent_all_ages <- as.integer(maindf$poverty_percent_all_ages)

# POVERTY QUARTILES
maindf$poverty_percent_all_ages <- as.numeric(as.character(maindf$poverty_percent_all_ages))
# quantile(maindf$poverty_percent_all_ages)
maindf$pov_quartile <- as.factor(ifelse(maindf$poverty_percent_all_ages <= 9.9, '1', 
                                 ifelse(maindf$poverty_percent_all_ages <= 12.8, '2', 
                                        ifelse(maindf$poverty_percent_all_ages <= 16.6, '3', 
                                               ifelse(maindf$poverty_percent_all_ages <= 43.9, '4')))))

# POVERTY GRAPHS
# overall poverty distribution w/in US
ggplot(data = maindf) +
  geom_density(mapping = aes(x = poverty_percent_all_ages))
# separate by quartiles 
ggplot(data = maindf) +
  geom_density(mapping = aes(x = poverty_percent_all_ages, color = pov_quartile))
# poverty percent and mortality ratio
ggplot(data = maindf) +
  geom_point(mapping = aes(x = poverty_percent_all_ages, y = ratio, color = pov_quartile))
# density of mortality rate by quartile (quartile 1 means richer area)
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = ratio, color = pov_quartile))

# subset poverty data to find mean of each quartile 
maindf1 <- maindf[maindf$pov_quartile == "1", ]
maindf1_mean <- mean(maindf1$ratio)
# mean = 0.01083488
maindf2 <- maindf[maindf$pov_quartile == "2", ]
maindf2_mean <- mean(maindf2$ratio)
# mean = 0.01387041
maindf3 <- maindf[maindf$pov_quartile == "3", ]
maindf3_mean <- mean(maindf3$ratio)
# mean = 0.01561862
maindf4 <- maindf[maindf$pov_quartile == "4", ]
maindf4_mean <- mean(maindf4$ratio)
# mean = 0.01701428
# notice: mortality rate increases as poverty quartile decreases

# MEDIAN HOUSEHOLD INCOME QUARTILES
# find quantiles 
maindf$median_household_income <- as.numeric(gsub(",", "", maindf$median_household_income))
quantile(maindf$median_household_income, na.rm = TRUE)
maindf$medhh_quartile <- as.factor(ifelse(maindf$median_household_income <= 47722, '1', 
                                              ifelse(maindf$median_household_income <= 55055, '2', 
                                                     ifelse(maindf$median_household_income <= 63963, '3',
                                                            ifelse(maindf$median_household_income <= 160305, '4')))))
# MHI GRAPHS
#plot mortality rate by med hh income quartiles
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = ratio, color = medhh_quartile))

#find avg mortality rate among different quartiles 
mean_medhh1 <- mean(maindf$ratio[maindf$medhh_quartile == '1'])
# mean = 0.01789561
mean_medhh2 <- mean(maindf$ratio[maindf$medhh_quartile == '2'])
# mean = 0.01573182
mean_medhh3 <- mean(maindf$ratio[maindf$medhh_quartile == '3'])
# mean = 0.01331643
mean_medhh4 <- mean(maindf$ratio[maindf$medhh_quartile == '4'])
# mean = 0.01032676
# notice: average mortality ratio decreases as income quartile increases

ggplot(data = maindf) +
  geom_point(mapping = aes(x = poverty_percent_all_ages, y = ratio, color = medhh_quartile))

ggplot(data = maindf) +
  geom_bar(mapping = aes(x = poverty_percent_all_ages, y = ratio, fill = medhh_quartile), 
           stat = "identity",
           position = "stack")

ggplot(data = maindf) +
  geom_tile(mapping = aes(x = pov_quartile, y = medhh_quartile, fill = ratio))

# UNEMPLOYMENT QUARTILES
maindf$unemp_percent <- as.numeric(as.character(maindf$unemp_percent))
quantile(maindf$unemp_percent)
maindf$unemp_quartile <- as.factor(ifelse(maindf$unemp_percent <= 3.5, '1', 
                                        ifelse(maindf$unemp_percent <= 4.4, '2', 
                                               ifelse(maindf$unemp_percent <= 5.5, '3', 
                                                      ifelse(maindf$unemp_percent <= 19.9, '4')))))
# unemployment graphs
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = ratio, color = unemp_quartile))

#find avg mortality rate among different unemployment rate groups 
mean_unemp1 <- mean(maindf$ratio[maindf$unemp_quartile == '1'])
# mean = 0.01354782
mean_unemp2 <- mean(maindf$ratio[maindf$unemp_quartile == '2'])
# mean = 0.01421827
mean_unemp3 <- mean(maindf$ratio[maindf$unemp_quartile == '3'])
# mean = 0.01437989
mean_unemp4 <- mean(maindf$ratio[maindf$unemp_quartile == '4'])
# mean = 0.01519225
# NOTICE mean mortality increases with unemployment rate but not by much
fact_rat <- as.factor(ifelse(maindf$ratio <= 0.010215378, '1', 
                                          ifelse(maindf$ratio <= 0.013470173, '2', 
                                                 ifelse(maindf$ratio <= .017370823, '3', 
                                                        ifelse(maindf$ratio <= 0.5, '4')))))
ggplot(data = maindf, aes(x = unemp_percent, y = poverty_percent_all_ages,
                          color = fact_rat, alpha = .5)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = maindf) +
  geom_bar(mapping = aes(x = fact_rat, fill = pov_quartile),
           position = "stack",
           nbins = 5,
           binwidth = 1)

ggplot(data = maindf) +
  geom_bar(mapping = aes(x = fact_rat, fill = medhh_quartile),
           position = "stack",
           nbins = 5,
           binwidth = 1)

ggplot(data = maindf) +
  geom_bar(mapping = aes(x = fact_rat, fill = unemp_quartile),
           position = "stack",
           nbins = 5,
           binwidth = 1)
