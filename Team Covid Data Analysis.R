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
racepop <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/DECENNIALPL2020.P2-2022-10-27T214708.csv", 
                    fill = TRUE)
#find sum of all deaths for each race in each state
raceagg <- aggregate(COVID.19.Deaths ~ State + Race.and.Hispanic.Origin.Group, data = race, FUN = sum)
#find total deaths of each race 
totaldeaths_race <- aggregate(COVID.19.Deaths ~ Race.and.Hispanic.Origin.Group, data = raceagg, FUN = sum)
racepop <- racepop[-c(1,11:73), ]
#convert necessary columns to numeric
racepop[2:53] <- lapply(racepop[2:53], gsub, pattern = ",", replacement = "")
racepop[2:53] <- lapply(racepop[2:53], as.numeric)
#add total deaths for each race
racepop$total <- rowSums(racepop[, 2:53])
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
mortality_rate_df <- dplyr::select(mortality_rate_df, FIPS, ratio)

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
pov <- dplyr::select(pov, county_fips_code, poverty_percent_all_ages, median_household_income)

# EMPLOYMENT

# EDUCATIONAL DATA
education <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/Education.csv", 
                      header = TRUE, fill = TRUE)
education <- education[-c(1:3), -c(2:43)]

#change column names 
names(education) <- c("FIPS", "no_HS", "HS", "some_college", "BD")
education <- education[-c(1), ]
education$FIPS <- as.numeric(education$FIPS)
education$no_HS <- as.numeric(education$no_HS)
education$HS <- as.numeric(education$HS)
education$some_college <- as.numeric(education$some_college)
education$BD <- as.numeric(education$BD)

# fix fips to include state code
employment$code <- paste(employment$code_2, employment$code_3)
employment$code <- gsub(" ", "", employment$code)
employment$code <- str_remove(employment$code, "^0+")
# keep only fips, unemployment percent
employment <- dplyr::select(employment, code, percent)
#Change FIPS to numeric to remove 0's

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


#Working w/ race data
names(totaldeaths_race)[names(totaldeaths_race) == 'Race.and.Hispanic.Origin.Group'] <- 'Race_Ethnicity'
names(racepop)[names(racepop) == 'Label..Grouping.'] <- 'Race_Ethnicity'
totaldeaths_race$Race_Ethnicity <- c("Hispanic_Latino", "NH_Native_American",
                                "NH_Asian", "NH_Black", "NH_mult", 
                                "NH_Hawaiian_PI", "NH_White", "Total", "Other")
racepop$Race_Ethnicity <- c("Hispanic_Latino","NH", "One_Race", "NH_White", "NH_Black", 
                       "NH_Native_American", "NH_Asian", "NH_Hawaiian_PI", "Other")
racepop <- racepop[-c(2,3), ]
totaldeaths_race <- totaldeaths_race[-c(5,8), ]

race_mr <- full_join(racepop, totaldeaths_race, by = "Race_Ethnicity")
#find mortality rate
race_mr$mortality_rate <- race_mr$COVID.19.Deaths/race_mr$total

# get rid of state columns 
race_total <- race_mr[, -c(2:53)]

#plot mortality rates 
ggplot(data = race_total) +
  geom_bar(mapping = aes(x = Race_Ethnicity, y = mortality_rate), stat ="identity")

#Education Data
#merge education, poverty, and mortality rate data
#change FIPS to numeric in pov_and_mr to join 
pov_and_mr$FIPS <- as.numeric(pov_and_mr$FIPS)
edu_pov_mr <- left_join(pov_and_mr, education, by = "FIPS")
edu_pov_mr <- na.omit(edu_pov_mr)
#test correlation between each education level
#didn't complete high school
ggplot(data = edu_pov_mr) +
  geom_point(mapping = aes(x = no_HS, y = ratio))

#completed high school
ggplot(data = edu_pov_mr) +
  geom_point(mapping = aes(x = HS, y = ratio))

#some college
ggplot(data = edu_pov_mr) +
  geom_point(mapping = aes(x = some_college, y = ratio))

#Bachelor's Degree
ggplot(data = edu_pov_mr) +
  geom_point(mapping = aes(x = BD, y = ratio))

#split into quantiles? 
quantile(edu_pov_mr$BD)
edu_pov_mr$BD_quartile <- as.factor(ifelse(edu_pov_mr$BD <= 15.3, '1',
                                           ifelse(edu_pov_mr$BD <= 19.5, '2', 
                                                  ifelse(edu_pov_mr$BD <= 26, '3', 
                                                         ifelse(edu_pov_mr$BD <= 77.6, '4'))))) 
#split ratio into quartiles
edu_pov_mr$ratio_quartile <- as.factor(ifelse(edu_pov_mr$ratio <= 0.010225385, '1',
                                           ifelse(edu_pov_mr$ratio <= 0.013472447, '2', 
                                                  ifelse(edu_pov_mr$ratio <= 0.017375413, '3', 
                                                         ifelse(edu_pov_mr$ratio <= 0.05, '4'))))) 

ggplot(data = edu_pov_mr) +
  geom_bar(mapping = aes(x = ratio_quartile, y = ..count.., fill = BD_quartile))

ggplot(data = edu_pov_mr) +
  geom_density(mapping = aes(x = ratio, color = BD_quartile))

ggplot(data = edu_pov_mr) +
  geom_histogram(mapping = aes(x = ratio, fill = BD_quartile))

ggplot(data = edu_pov_mr) +
  geom_boxplot(mapping = aes(x = BD_quartile, y = ratio))

#median mortality rate among different quartiles 
BD_one_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "1"])

BD_two_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "2"])

BD_three_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "3"])

BD_four_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "4"])

#attempt to find mortality rate for each day
# get rid of unnecessary columns from deaths and cases
deaths <- deaths[, -c(1:4, 6, 8:54)]
cases <- cases[, -c(1:4, 6, 8:53)]
#join data frames 
mr_df <- left_join(cases,deaths, by = c("FIPS", "Province_State"))
mr_df <- na.omit(mr_df)                 

#column 960: start of deaths
#column 3: start of cases
deaths <- na.omit(deaths)
cases <- na.omit(cases)
mr_trend <- data.frame(mr_df$FIPS, mr_df$Province_State)
for(i in 3:ncol(cases)){
  x <- deaths[[i]]
  y <- cases[[i]]
  new <- x/y
  mr_trend[, ncol(mr_trend) + 1] <- new
  colnames(mr_trend)[ncol(mr_trend)] <- paste0("new", i)
}

mr_trend[is.na(mr_trend)] <- 0

names(mr_trend)[names(mr_trend) == "mr_df.FIPS"] <- "FIPS"
#remove Province_State 
mr_trend <- mr_trend[, -c(2)]
#change FIPS to character for mr_trend 
mr_trend$FIPS <- as.character(mr_trend$FIPS)
mr_trend_pov <- left_join(pov, mr_trend, by = "FIPS")
mr_trend_pov <- na.omit(mr_trend_pov)

#aggregate df to find avg mortality rate each day w/in the quartiles
mr_trend_agg <- aggregate(mr_trend_pov, by = list(mr_trend_pov$quartile), FUN = mean)
is.na(mr_trend_agg) <- sapply(mr_trend_agg, is.infinite)
mr_trend_agg[is.na(mr_trend_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose <- t(mr_trend_agg)
transpose <- transpose[-c(1:5), ]
colnames(transpose) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

dates <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/Dates.csv", 
                  header = FALSE)
colnames(dates) <- c("Date")
bind <- cbind(dates, transpose)
bind$Date <- as.Date(bind$Date)
bind$Quartile_1 <- as.numeric(bind$Quartile_1)
bind$Quartile_2 <- as.numeric(bind$Quartile_2)
bind$Quartile_3 <- as.numeric(bind$Quartile_3)
bind$Quartile_4 <- as.numeric(bind$Quartile_4)
ggplot(data = bind) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_1, color = "1")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_2, color = "2")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_3, color = "3")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_4, color = "4")) +
  scale_color_manual(name = "Quartile", 
                     breaks = c("1", "2", "3", 
                                "4"), 
                     values = c("1" = 1, "2" = 2, 
                                "3" = 3, "4" = 4)) +
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 7)) +
  labs(x = "Date", y = "Average Mortality Rate", main = "Mortality Rate By Quartile")

