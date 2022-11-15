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
race <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/race.csv", 
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
# create new column to create normalized density plot
pov_and_mr$ratio <- unlist(pov_and_mr$ratio)
ggplot(data = pov_and_mr, aes(ratio, color = quartile)) + 
  geom_density() +
  labs(x = "Mortality Rate", y = "Density", 
       title = "Mortality Rate Density Curve", 
       color = "Poverty Percent Quartile")

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
  geom_density(mapping = aes(x = ratio, color = medhh_quartile)) +
  labs(x = "Mortality Rate", y = "Density", 
       title = "Mortality Rate Density", 
       color = "Med. HH Income Quartile")

#find avg mortality rate among different quartiles 
mean_medhh1 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '1'])
# mean = 0.01789561
mean_medhh2 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '2'])
# mean = 0.01573182
mean_medhh3 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '3'])
# mean = 0.01331643
mean_medhh4 <- mean(pov_and_mr$ratio[pov_and_mr$medhh_quartile == '4'])
# mean = 0.01032676

#Education Data
#merge education, poverty, and mortality rate data
#change FIPS to numeric in pov_and_mr to join 
pov_and_mr$FIPS <- as.numeric(pov_and_mr$FIPS)
edu_pov_mr <- left_join(pov_and_mr, education, by = "FIPS")
edu_pov_mr <- na.omit(edu_pov_mr)
#test correlation between each education level
#didn't complete high school
ggplot(data = edu_pov_mr, aes(x = no_HS, y = ratio)) +
  geom_point() +
  geom_jitter(alpha = 0.5)

#completed high school
ggplot(data = edu_pov_mr, aes(x = HS, y = ratio)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "% of County that Completed High School", y = "Mortality Rate", 
       title = "Mortality Rate vs Percent of County that Completed High school") +
  theme(plot.title = element_text(hjust = 0.5))

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
  geom_bar(mapping = aes(x = ratio_quartile, y = ..count.., fill = BD_quartile)) +
  labs(x = "Mortality Rate Quartile", y = "Count", title = "Mortality Rate by Bachelor's Quartile", 
       fill = "Bachelor's Degree\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("mr quartile by BD Bar.png", 
       plot = last_plot(), width = 5.5, height = 4)

ggplot(data = edu_pov_mr) +
  geom_density(mapping = aes(x = ratio, color = BD_quartile)) +
  labs(x = "Mortality Rate", y = "Density", title = "Mortality Rate Density", 
       color = "Bachelor Degree\n Quartile") +
  theme(plot.title = element_text(hjust = 0.5))
mean(pov_and_mr$ratio)

ggplot(data = edu_pov_mr) +
  geom_histogram(mapping = aes(x = ratio, fill = BD_quartile))

ggplot(data = edu_pov_mr) +
  geom_boxplot(mapping = aes(x = BD_quartile, y = ratio, color = BD_quartile)) +
  labs(x = "Bachelor Degree Quartile", y = "Mortality Rate", 
       title = "Mortality Rate Distribution by Bachelor's Degree Quartile", 
       color = "Bachelor's Degree\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("mortality rate distribution by BD BP.png", 
       plot = last_plot(), width = 5.5, height = 4)


  
#median mortality rate among different quartiles 
BD_one_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "1"])

BD_two_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "2"])

BD_three_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "3"])

BD_four_med <- median(edu_pov_mr$ratio[edu_pov_mr$BD_quartile == "4"])

#attempt to find mortality rate for each day
# get rid of unnecessary columns from deaths and cases
deaths <- deaths[, -c(1:4, 6, 8:54)]
cases <- cases[, -c(1:4, 6, 8:53)]

#column 960: start of deaths
#column 3: start of cases
deaths <- na.omit(deaths)
cases <- na.omit(cases)
mr_trend <- data.frame(deaths$FIPS, deaths$Province_State)
for(i in 3:ncol(cases)){
  x <- deaths[[i]]
  y <- cases[[i]]
  new <- x/y
  mr_trend[, ncol(mr_trend) + 1] <- new
  colnames(mr_trend)[ncol(mr_trend)] <- paste0("new", i)
}

mr_trend[is.na(mr_trend)] <- 0

names(mr_trend)[names(mr_trend) == "deaths.FIPS"] <- "FIPS"
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
  scale_color_manual(name = "Poverty Quartile", 
                     breaks = c("1", "2", "3", 
                                "4"), 
                     values = c("1" = 1, "2" = 2, 
                                "3" = 3, "4" = 4)) +
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) +
  labs(x = "Date", y = "Average Mortality Rate", 
       title = "Mortality Rate Trend") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))
ggsave("mortality rate trend by POV.png", 
       plot = last_plot(), width = 5.5, height = 4)

#RACE 
race <- race[race$YEAR == 12, ]
racevars <- c("SUMLEV", "STATE", "COUNTY", "CTYNAME", "AGEGRP", "TOT_POP",
              "WA_MALE", "WA_FEMALE",
              "BA_MALE", "BA_FEMALE",
              "IA_MALE", "IA_FEMALE",
              "AA_MALE", "AA_FEMALE",
              "NA_MALE", "NA_FEMALE",
              "H_MALE", "H_FEMALE")
race <- race[racevars]
# create total race population value
race$White <- race$WA_MALE + race$WA_FEMALE
race$Black <- race$BA_MALE + race$BA_FEMALE
race$Native <- race$IA_MALE + race$IA_FEMALE
race$Asian <- race$AA_MALE + race$AA_FEMALE
race$Hawaiian <- race$NA_MALE + race$NA_FEMALE
race$Hispanic <- race$H_MALE + race$H_FEMALE
# keep the total race populations; get rid of populations by gender
racevars2 <- c("STATE", "COUNTY", "CTYNAME", "AGEGRP", "TOT_POP",
               "White",
               "Black",
               "Native",
               "Asian",
               "Hawaiian",
               "Hispanic")
race <- race[racevars2]
# create total race percentages
race$White_perc <- race$White / race$TOT_POP
race$Black_perc <- race$Black / race$TOT_POP
race$Native_perc <- race$Native / race$TOT_POP
race$Asian_perc <- race$Asian / race$TOT_POP
race$Hawaiian_perc <- race$Hawaiian / race$TOT_POP
race$Hispanic_perc <- race$Hispanic / race$TOT_POP
racevars3 <- c("STATE", "COUNTY", "CTYNAME", "AGEGRP", "TOT_POP",
               "White_perc",
               "Black_perc",
               "Native_perc",
               "Asian_perc",
               "Hawaiian_perc",
               "Hispanic_perc")
race <- race[racevars3]
# create fips
race$COUNTY <- str_pad(race$COUNTY, width = 3, pad = "0")
race$FIPS <- paste(as.character(race$STATE), race$COUNTY)
race$FIPS <- gsub(" ", "", race$FIPS)
racevars4 <- c("AGEGRP", "TOT_POP",
               "White_perc",
               "Black_perc",
               "Native_perc",
              "Asian_perc",
               "Hawaiian_perc",
               "Hispanic_perc", "FIPS")
race <- race[racevars4]
# only keep the TOTAL age groups
# should also consider other age groups later 
race <- race[race$AGEGRP == 0, ]
racevars5 <- c("FIPS", "TOT_POP",
               "White_perc",
               "Black_perc",
               "Native_perc",
               "Asian_perc",
               "Hawaiian_perc",
               "Hispanic_perc")
race <- race[racevars5]

#Find white quartile
quantile(race$White_perc)
#create new column with white quartiles 
race$white_quartile <- as.factor(ifelse(race$White_perc <= 0.79520388, '1', 
                                        ifelse(race$White_perc <= .91262314, '2', 
                                               ifelse(race$White_perc <= 0.95411157, '3', 
                                                      ifelse(race$White_perc <= 1, '4')))))
#Find black quartiles
quantile(race$Black_perc)
race$black_quartile <- as.factor(ifelse(race$Black_perc <= 0.008927714, '1', 
                                        ifelse(race$Black_perc <= 0.025427597, '2', 
                                               ifelse(race$Black_perc <= 0.108602869, '3', 
                                                      ifelse(race$Black_perc <= 1, '4')))))
#Find hispanic quartiles
quantile(race$Hispanic_perc)
race$hispanic_quartile <- as.factor(ifelse(race$Hispanic_perc <= 0.024699270, '1', 
                                           ifelse(race$Hispanic_perc <= 0.044783246, '2', 
                                                  ifelse(race$Hispanic_perc <= 0.101442643, '3', 
                                                         ifelse(race$Hispanic_perc <= 1, '4')))))
#Find asian quartiles 
quantile(race$Asian_perc)
race$asian_quartile <- as.factor(ifelse(race$Asian_perc <= 0.004774536, '1', 
                                        ifelse(race$Asian_perc <= 0.007524692, '2', 
                                               ifelse(race$Asian_perc <= 0.014507827, '3', 
                                                      ifelse(race$Asian_perc <= 1, '4')))))

#create race quartile trend graphs 
mr_trend_race <- left_join(race, mr_trend, by = "FIPS")
mr_white_agg <- aggregate(mr_trend_race, by = list(mr_trend_race$white_quartile), FUN = mean)
mr_white_agg <- mr_white_agg[, -c(2,3,5:13)]
is.na(mr_white_agg) <- sapply(mr_white_agg, is.infinite)
mr_white_agg[is.na(mr_white_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_white <- t(mr_white_agg)
transpose_white <- transpose_white[-c(1:2), ]
colnames(transpose_white) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

bind_white <- cbind(dates, transpose_white)
bind_white$Date <- as.Date(bind_white$Date)
bind_white$Quartile_1 <- as.numeric(bind_white$Quartile_1)
bind_white$Quartile_2 <- as.numeric(bind_white$Quartile_2)
bind_white$Quartile_3 <- as.numeric(bind_white$Quartile_3)
bind_white$Quartile_4 <- as.numeric(bind_white$Quartile_4)

ggplot(data = bind_white) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_1, color = "1")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_2, color = "2")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_3, color = "3")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_4, color = "4")) +
  scale_color_manual(name = "White\nQuartile", 
                     breaks = c("1", "2", "3", 
                                "4"), 
                     values = c("1" = 1, "2" = 2, 
                                "3" = 3, "4" = 4)) +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  labs(x = "Date", y = "Average Mortality Rate", title = "Mortality Rate Trend")
ggsave("mortality rate distribution white trend.png", 
       plot = last_plot(), width = 5.5, height = 4)


# Black 
mr_trend_race <- left_join(race, mr_trend, by = "FIPS")
mr_black_agg <- aggregate(mr_trend_race, by = list(mr_trend_race$black_quartile), FUN = mean)
mr_black_agg <- mr_black_agg[, -c(2:4,6:13)]
is.na(mr_black_agg) <- sapply(mr_black_agg, is.infinite)
mr_black_agg[is.na(mr_black_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_black <- t(mr_black_agg)
transpose_black <- transpose_black[-c(1:2), ]
colnames(transpose_black) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

bind_black <- cbind(dates, transpose_black)
bind_black$Date <- as.Date(bind_black$Date)
bind_black$Quartile_1 <- as.numeric(bind_black$Quartile_1)
bind_black$Quartile_2 <- as.numeric(bind_black$Quartile_2)
bind_black$Quartile_3 <- as.numeric(bind_black$Quartile_3)
bind_black$Quartile_4 <- as.numeric(bind_black$Quartile_4)

ggplot(data = bind_black) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_1, color = "1")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_2, color = "2")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_3, color = "3")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_4, color = "4")) +
  scale_color_manual(name = "Black\nQuartile", 
                     breaks = c("1", "2", "3", 
                                "4"), 
                     values = c("1" = 1, "2" = 2, 
                                "3" = 3, "4" = 4)) +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  labs(x = "Date", y = "Average Mortality Rate", title = "Mortality Rate Trend")
ggsave("mortality rate distribution black trend.png", 
       plot = last_plot(), width = 5.5, height = 4)

# Hispanic Quartile
mr_hispanic_agg <- aggregate(mr_trend_race, by = list(mr_trend_race$hispanic_quartile), FUN = mean)
mr_hispanic_agg <- mr_hispanic_agg[, -c(2:8,10:13)]
is.na(mr_hispanic_agg) <- sapply(mr_hispanic_agg, is.infinite)
mr_hispanic_agg[is.na(mr_hispanic_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_hispanic <- t(mr_hispanic_agg)
transpose_hispanic <- transpose_hispanic[-c(1:2), ]
colnames(transpose_hispanic) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

bind_hispanic <- cbind(dates, transpose_hispanic)
bind_hispanic$Date <- as.Date(bind_hispanic$Date)
bind_hispanic$Quartile_1 <- as.numeric(bind_hispanic$Quartile_1)
bind_hispanic$Quartile_2 <- as.numeric(bind_hispanic$Quartile_2)
bind_hispanic$Quartile_3 <- as.numeric(bind_hispanic$Quartile_3)
bind_hispanic$Quartile_4 <- as.numeric(bind_hispanic$Quartile_4)

ggplot(data = bind_hispanic) +
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
  labs(x = "Date", y = "Average Mortality Rate", title = "Mortality Rate By Hispanic Quartile")


# Asian
mr_asian_agg <- aggregate(mr_trend_race, by = list(mr_trend_race$asian_quartile), FUN = mean)
mr_asian_agg <- mr_asian_agg[, -c(2:8,10:13)]
is.na(mr_asian_agg) <- sapply(mr_asian_agg, is.infinite)
mr_asian_agg[is.na(mr_asian_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_asian <- t(mr_asian_agg)
transpose_asian <- transpose_asian[-c(1:2), ]
colnames(transpose_asian) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

bind_asian <- cbind(dates, transpose_asian)
bind_asian$Date <- as.Date(bind_asian$Date)
bind_asian$Quartile_1 <- as.numeric(bind_asian$Quartile_1)
bind_asian$Quartile_2 <- as.numeric(bind_asian$Quartile_2)
bind_asian$Quartile_3 <- as.numeric(bind_asian$Quartile_3)
bind_asian$Quartile_4 <- as.numeric(bind_asian$Quartile_4)

ggplot(data = bind_asian) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_1, color = "1")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_2, color = "2")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_3, color = "3")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_4, color = "4")) +
  scale_color_manual(name = "Asian\nQuartile", 
                     breaks = c("1", "2", "3", 
                                "4"), 
                     values = c("1" = 1, "2" = 2, 
                                "3" = 3, "4" = 4)) +
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 7)) +
  labs(x = "Date", y = "Average Mortality Rate", 
       title = "Mortality Rate Trend") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))

  
ggplot(data = edu_pov_mr) +
  geom_boxplot(mapping = aes(x = quartile, y = ratio, color = quartile)) +
  labs(x = "Poverty Percent Quartile", y = "Mortality Rate", 
       title = "Mortality Rate Distribution by Poverty Quartile", 
       color = "Poverty Percent Quartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("mortality rate distribution by POV BP.png", 
       plot = last_plot(), width = 5.5, height = 4)
#find median of each poverty quartile 
median(pov_and_mr1$ratio)
median(pov_and_mr2$ratio)  
median(pov_and_mr3$ratio)
median(pov_and_mr4$ratio)
median(pov_and_mr$ratio)

(median(pov_and_mr4$ratio) - median(pov_and_mr$ratio))/median(pov_and_mr$ratio)

#find mean of each poverty quartile 
mean(pov_and_mr1$ratio)
mean(pov_and_mr2$ratio)  
mean(pov_and_mr3$ratio)
mean(pov_and_mr4$ratio)
mean(pov_and_mr$ratio)
t.test(pov_and_mr1$ratio, mu = mean(pov_and_mr$ratio))

#Population by county data set 
population <- race[, c(1,2)]
population$FIPS <- as.numeric(population$FIPS)
recent_cases <- cases[, c(1,375)]
inf_rate <- left_join(recent_cases, population, by = "FIPS")
inf_rate <- na.omit(inf_rate)
inf_rate$infection_ratio <- inf_rate$X3.11.21 / inf_rate$TOT_POP
all_df <- left_join(edu_pov_mr, inf_rate, by = "FIPS")
all_df <- all_df[ , -c(13, 14)]
all_df <- na.omit(all_df)
#Box plots for infection rate 
ggplot(data = all_df) +
  geom_boxplot(mapping = aes(x = quartile, y = infection_ratio, 
                             color = quartile)) + 
  labs(x = "Poverty Quartile", y = "Infection Rate", 
       title = "Infection Rate by Poverty Quartile", color = "Poverty\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by POV BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

ggplot(data = all_df) +
  geom_boxplot(mapping = aes(x = BD_quartile, y = infection_ratio, 
                             color = BD_quartile)) + 
  labs(x = "Bachelor's Quartile", y = "Infection Rate", 
       title = "Infection Rate by Bachelor's Quartile", color = "Bachelor's\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by BD BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

raceq <- race[, c(1, 9:12)]
raceq$FIPS <- as.numeric(raceq$FIPS)
all_df <- left_join(all_df, raceq, by = "FIPS")

ggplot(data = all_df) +
  geom_boxplot(mapping = aes(x = white_quartile, y = infection_ratio, 
                             color = white_quartile)) + 
  labs(x = "White Quartile", y = "Infection Rate", 
       title = "Infection Rate by White Quartile", color = "White\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by white BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

ggplot(data = all_df) +
  geom_boxplot(mapping = aes(x = black_quartile, y = infection_ratio, 
                             color = black_quartile)) + 
  labs(x = "Black Quartile", y = "Infection Rate", 
       title = "Infection Rate by Black Quartile", color = "Black\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by black BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

#Trend of Infection Rate 
temp <- left_join(cases, population, by = "FIPS")
temp <- na.omit(temp)
temp_cases <- temp[, -c(1,2,960)]
ir_trend <- data.frame(temp$FIPS)
for(i in 1:ncol(temp_cases)){
  x <- temp_cases[[i]]
  y <- temp$TOT_POP
  new <- x/y
  ir_trend[, ncol(ir_trend) + 1] <- new
  colnames(ir_trend)[ncol(ir_trend)] <- paste0("new", i)
}
#infection rate trend by POV Quartile 
ir_trend[is.na(ir_trend)] <- 0

names(ir_trend)[names(ir_trend) == "temp.FIPS"] <- "FIPS"
#change FIPS to character for mr_trend 
ir_trend$FIPS <- as.character(ir_trend$FIPS)
ir_trend <- ir_trend[, -c(1913)]
ir_trend_pov <- left_join(pov, ir_trend, by = "FIPS")
ir_trend_pov <- na.omit(ir_trend_pov)

#aggregate df to find avg mortality rate each day w/in the quartiles
ir_trend_agg <- aggregate(ir_trend_pov, by = list(ir_trend_pov$quartile), FUN = mean)
is.na(ir_trend_agg) <- sapply(ir_trend_agg, is.infinite)
ir_trend_agg[is.na(ir_trend_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_ir <- t(ir_trend_agg)
transpose_ir <- transpose_ir[-c(1:5), ]
colnames(transpose_ir) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")
bind_ir <- cbind(dates, transpose_ir)
bind_ir$Date <- as.Date(bind_ir$Date)
bind_ir$Quartile_1 <- as.numeric(bind_ir$Quartile_1)
bind_ir$Quartile_2 <- as.numeric(bind_ir$Quartile_2)
bind_ir$Quartile_3 <- as.numeric(bind_ir$Quartile_3)
bind_ir$Quartile_4 <- as.numeric(bind_ir$Quartile_4)

ggplot(data = bind_ir) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_1, color = "1")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_2, color = "2")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_3, color = "3")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_4, color = "4")) +
  scale_color_manual(name = "Poverty Quartile", 
                     breaks = c("1", "2", "3", 
                                "4"), 
                     values = c("1" = 1, "2" = 2, 
                                "3" = 3, "4" = 4)) +
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) +
  labs(x = "Date", y = "Average Infection Rate", 
       title = "Infection Rate Trend") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))
ggsave("infection rate trend by POV.png", 
       plot = last_plot(), width = 5.5, height = 4)
#density plots infection rate 
ggplot(data = all_df) +
  geom_density(mapping = aes(x = infection_ratio, color = quartile)) +
  labs(x = "Infection Rate", y = "Density", title = "Infection Rate Density", 
       color = "Poverty\n Quartile") +
  theme(plot.title = element_text(hjust = 0.5))

#more recent infection rate 
oct_cases <- cases[, c(1,959)]
inf_rateOct <- left_join(oct_cases, population, by = "FIPS")
inf_rateOct <- na.omit(inf_rateOct)
inf_rateOct$infection_ratio <- inf_rateOct$X10.16.22 / inf_rateOct$TOT_POP
dfOct <- left_join(edu_pov_mr, inf_rateOct, by = "FIPS")
dfOct <- dfOct[ , -c(13, 14)]
dfOct <- na.omit(dfOct)

#Box plot for October Infection rate 
ggplot(data = dfOct) +
  geom_boxplot(mapping = aes(x = quartile, y = infection_ratio, 
                             color = quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "Poverty Quartile", y = "Infection Rate", 
       title = "Infection Rate by Poverty Quartile", color = "Poverty\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by POV oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

ggplot(data = dfOct) +
  geom_boxplot(mapping = aes(x = BD_quartile, y = infection_ratio, 
                             color = BD_quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "Bachelor's Quartile", y = "Infection Rate", 
       title = "Infection Rate by Bachelor's Quartile", color = "Bachelor's\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by BD oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

dfOct <- left_join(dfOct, raceq, by = "FIPS")
dfOct <- na.omit(dfOct)

ggplot(data = dfOct) +
  geom_boxplot(mapping = aes(x = white_quartile, y = infection_ratio, 
                             color = white_quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "White Quartile", y = "Infection Rate", 
       title = "Infection Rate by White Quartile", color = "White\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by white oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

ggplot(data = dfOct) +
  geom_boxplot(mapping = aes(x = black_quartile, y = infection_ratio, 
                             color = black_quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "Black Quartile", y = "Infection Rate", 
       title = "Infection Rate by Black Quartile", color = "Black\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by black oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

#subset education quartiles 
bd_mr1 <- all_df[all_df$BD_quartile == "1", ]
bd_mr2 <- all_df[all_df$BD_quartile == "2", ]
bd_mr3 <- all_df[all_df$BD_quartile == "3", ]
bd_mr4 <- all_df[all_df$BD_quartile == "4", ]

median(bd_mr1$ratio)
median(bd_mr2$ratio)
median(bd_mr3$ratio)
median(bd_mr4$ratio)
abs((median(bd_mr4$ratio) - median(all_df$ratio))) / median(all_df$ratio)
abs((median(bd_mr4$ratio) - median(bd_mr1$ratio))) / median(bd_mr1$ratio)


mean(bd_mr1$ratio)
mean(bd_mr2$ratio)
mean(bd_mr3$ratio)
mean(bd_mr4$ratio)


t.test(bd_mr4$ratio, mu = mean(all_df$ratio))

median(pov_and_mr1$poverty_percent_all_ages)

ggplot(data = all_df) +
  geom_bar(mapping = aes(x = poverty_percent_all_ages, y = ratio))

mr_trend$FIPS <- as.numeric(mr_trend$FIPS)
mr_trend_BD <- left_join(all_df, mr_trend, by = "FIPS")
mr_trend_BD <- na.omit(mr_trend_BD)

# trend by education level

#aggregate df to find avg mortality rate each day w/in the quartiles
mr_trend_BD <- aggregate(mr_trend_BD, by = list(mr_trend_BD$BD_quartile), FUN = mean)
is.na(mr_trend_BD) <- sapply(mr_trend_BD, is.infinite)
mr_trend_BD[is.na(mr_trend_BD)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_BD <- t(mr_trend_BD)
transpose_BD <- transpose_BD[-c(1:18), ]
colnames(transpose_BD) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

bind_BD <- cbind(dates, transpose_BD)
bind_BD$Date <- as.Date(bind_BD$Date)
bind_BD$Quartile_1 <- as.numeric(bind_BD$Quartile_1)
bind_BD$Quartile_2 <- as.numeric(bind_BD$Quartile_2)
bind_BD$Quartile_3 <- as.numeric(bind_BD$Quartile_3)
bind_BD$Quartile_4 <- as.numeric(bind_BD$Quartile_4)

ggplot(data = bind_BD) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_1, color = "1")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_2, color = "2")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_3, color = "3")) +
  geom_smooth(mapping = aes(x = Date, y = Quartile_4, color = "4")) +
  scale_color_manual(name = "Bachelor's Quartile", 
                     breaks = c("1", "2", "3", 
                                "4"), 
                     values = c("1" = 1, "2" = 2, 
                                "3" = 3, "4" = 4)) +
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) +
  labs(x = "Date", y = "Average Mortality Rate", 
       title = "Mortality Rate Trend") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))
ggsave("mortality rate trend by BD.png", 
       plot = last_plot(), width = 5.5, height = 4)

all_df$ir_quartile <- as.factor(ifelse(all_df$infection_ratio <= 0.075010194, '1', 
                                 ifelse(all_df$infection_ratio <= 0.092227808, '2', 
                                        ifelse(all_df$infection_ratio <= 0.109401136, '3', 
                                               ifelse(all_df$infection_ratio <= 0.347467415, '4')))))
ggplot(data = all_df) +
  geom_bar(mapping = aes(x = BD_quartile, y = ..count.., fill = ir_quartile)) +
  labs(x = "Infection Rate Quartile", y = "Count", title = "Bachelor's Degree by Infection Rate", 
       fill = "Infection Rate\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("bd quartile by ir Bar.png", 
       plot = last_plot(), width = 5.5, height = 4)

#calculate confidence interval for education 
se <- sd(all_df$ratio) / sqrt(nrow(bd_mr4))
sample_mean <- mean(all_df$ratio)
lower_bound <- sample_mean - 1.96*se
upper_bound <- sample_mean + 1.96*se
mean(bd_mr4$ratio)
# lower bound is 1.38
# Upper bound is 1.477
# mean is 1.03
# find z score 
pnorm(0.01030835, mean = sample_mean, sd = se)
z_BD4 <- (mean(all_df$ratio) - mean(bd_mr4$ratio)) / se

(mean(all_df$ratio) - mean(bd_mr4$ratio)) / mean(all_df$ratio)
