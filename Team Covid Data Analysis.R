library(ggplot2)
library(janitor)
library(dplyr)
library(stringr)
cases <- read.table("C:/Users/carly/OneDrive/Documents/R/Data/confirmedcases.txt", 
                                 quote = "\"", header = TRUE, sep = ",", fill = TRUE)
deaths <- read.table("C:/Users/carly/OneDrive/Documents/R/Data/deaths.txt", 
                        quote = "\"", header = TRUE, sep = ",", fill = TRUE)

# MORTALITY RATE DF
mortality_rate_df <- cases[c(1:11, length(cases))]
mortality_rate_df$cases <- cases[c(length(cases))]
mortality_rate_df$deaths <- deaths[c(length(cases))]
mortality_rate_df$ratio <- mortality_rate_df$deaths / mortality_rate_df$cases

# RACE
race <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/race_data.csv",
                 fill = TRUE)

# EMPLOYMENT
employment <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/laucnty21.csv",
                       fill = TRUE)
employment <- row_to_names(employment, 4, remove_rows_above = TRUE)
employment <- clean_names(employment)
employment <- employment[-1,]

# UID reference
uidref <- read.table("C:/Users/carly/OneDrive/Documents/R/Data/UID_lookup.txt",
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)
uidref <- uidref[which(uidref$iso3 == "USA"),]
uidref <- uidref[-c(2:177),]

# POVERTY 
pov <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/poverty_data.csv", header = FALSE)
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

# EMPLOYMENT

# fix fips to include state code
employment$code <- paste(employment$code_2, employment$code_3)
employment$code <- gsub(" ", "", employment$code)
employment$code <- str_remove(employment$code, "^0+")
# keep only fips, unemployment percent
employment <- select(employment, code, percent)


#separate into quartiles
pov$poverty_percent_all_ages <- as.numeric(as.character(pov$poverty_percent_all_ages))
pov <- pov[-c(551), ]
#find quantiles 
quantile(pov$poverty_percent_all_ages)
pov$quartile <- as.factor(ifelse(pov$poverty_percent_all_ages <= 9.9, '1', 
                          ifelse(pov$poverty_percent_all_ages <= 12.8, '2', 
                          ifelse(pov$poverty_percent_all_ages <= 16.6, '3', 
                          ifelse(pov$poverty_percent_all_ages <= 43.9, '4')))))
#graph poverty distribution w/in US
#overall poverty distribution w/in US
ggplot(data = pov) +
  geom_density(mapping = aes(x = poverty_percent_all_ages))
#separate by quartiles 
ggplot(data = pov) +
  geom_density(mapping = aes(x = poverty_percent_all_ages, color = quartile))
#change pov name for FIPS

names(pov)[names(pov) == 'county_fips_code'] <- 'FIPS'
#change mortality rate df FIPS to character 
mortality_rate_df$FIPS <- as.character(mortality_rate_df$FIPS)
#join poverty data and mortality rate 
pov_and_mr <- left_join(mortality_rate_df, pov, by = "FIPS")
#remove NAs
pov_and_mr <- na.omit(pov_and_mr)
#graph density of mortality rate by quartile (quartile 1 means richer area)
#unlist ratio? 
pov_and_mr$ratio <- unlist(pov_and_mr$ratio)
ggplot(data = pov_and_mr) + 
  geom_density(mapping = aes(x = ratio, color = quartile))

#see if correlation between poverty percent and mortality rate
ggplot(data = pov_and_mr) + 
  geom_point(mapping = aes(x = poverty_percent_all_ages, y = ratio, 
                           color = quartile))
#subset poverty data to find mean of each quartile 
pov_and_mr1 <- pov_and_mr[pov_and_mr$quartile == "1", ]
quartile_1mean <- mean(pov_and_mr1$ratio)
# mean = 0.01083488
pov_and_mr2 <- pov_and_mr[pov_and_mr$quartile == "2", ]
quartile_2mean <- mean(pov_and_mr2$ratio)
# mean = 0.01387041
pov_and_mr3 <- pov_and_mr[pov_and_mr$quartile == "3", ]
quartile_3mean <- mean(pov_and_mr3$ratio)
# mean = 0.01561862
pov_and_mr4 <- pov_and_mr[pov_and_mr$quartile == "4", ]
quartile_4mean <- mean(pov_and_mr4$ratio)
# mean = 0.01701428

#Working w/ median household income data 
#change median household income to numeric vector 
pov_and_mr$median_household_income <- as.numeric(gsub(",", "", pov_and_mr$median_household_income))
#distribution of med hh income in US
ggplot(data = pov_and_mr) + 
  geom_density(mapping = aes(x = median_household_income))

#separate hh income into quartiles 
#find quantiles 
quantile(pov_and_mr$median_household_income)
pov_and_mr$medhh_quartile <- as.factor(ifelse(pov_and_mr$median_household_income <= 47722, '1', 
                                       ifelse(pov_and_mr$median_household_income <= 55055, '2', 
                                       ifelse(pov_and_mr$median_household_income <= 63963, '3',
                                       ifelse(pov_and_mr$median_household_income <= 160305, '4')))))
#plot mortality rate by med hh income quartiles
ggplot(data = pov_and_mr) + 
  geom_density(mapping = aes(x = ratio, color = medhh_quartile))

#find avg mortality rate among different quartiles 
mean_medhh1 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '1'])
# mean = 0.01789561
mean_medhh2 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '2'])
# mean = 0.01573182
mean_medhh3 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '3'])
# mean = 0.01331643
mean_medhh4 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '4'])
# mean = 0.01032676