library(ggplot2)
library(janitor)
library(dplyr)
library(stringr)
library(data.table)
library(corrplot)

# IMPORT DATAFRAMES
cases <- read.table("C:/Users/carly/OneDrive/Documents/R/Data/confirmedcases.txt", 
                                 quote = "\"", header = TRUE, sep = ",", fill = TRUE)
deaths <- read.table("C:/Users/carly/OneDrive/Documents/R/Data/deaths.txt", 
                        quote = "\"", header = TRUE, sep = ",", fill = TRUE)

# MORTALITY RATE DF
mortality_rate_df <- cases[c(1:11, length(cases))]
mortality_rate_df$cases <- cases[c(length(cases))]
mortality_rate_df$deaths <- deaths[c(length(cases))]
mortality_rate_df$ratio <- mortality_rate_df$deaths / mortality_rate_df$cases



# POVERTY 
pov <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/poverty_data.csv", header = FALSE)
pov <- row_to_names(pov, 4, remove_rows_above = TRUE)
pov <- clean_names(pov)
pov <- pov[-1,]

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


#Extract Longitude and Latitude from Cases 
latlong <- dplyr::select(cases, FIPS, Lat, Long_)
latlong <- rename(cases, long = Long_)
latlong$FIPS <- as.character(latlong$FIPS)
latlong <- dplyr::select(latlong, FIPS, Lat, long)









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
pov <- dplyr::select(pov, county_fips_code, poverty_percent_all_ages, median_household_income)
pov <- na.omit(pov)
pov$poverty_percent_all_ages <- as.numeric(as.character(pov$poverty_percent_all_ages))
pov$median_household_income <- as.numeric(gsub(",", "", pov$median_household_income))
pov <- rename(pov, FIPS = county_fips_code)




# EDUCATIONAL DATA
edu <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/Education.csv", 
                      header = TRUE, fill = TRUE)
edu <- row_to_names(edu, 4, remove_rows_above = TRUE)
edu <- clean_names(edu)
edu <- edu[-1,]
# add fips 
edu$FIPS <- sub("^0+", "", edu$fips_code) 
# filter out relevant info and rename it
edu <- dplyr::select(edu, FIPS, percent_of_adults_with_less_than_a_high_school_diploma_2015_19,
              percent_of_adults_with_a_high_school_diploma_only_2015_19,
              percent_of_adults_with_a_bachelors_degree_or_higher_2015_19)
edu <- rename(edu, no_HS = percent_of_adults_with_less_than_a_high_school_diploma_2015_19,
              HS = percent_of_adults_with_a_high_school_diploma_only_2015_19,
              bachelors = percent_of_adults_with_a_bachelors_degree_or_higher_2015_19)
edu$bachelors <- as.integer(edu$bachelors)

#change to numeric vectors 
edu$no_HS <- as.numeric(edu$no_HS)
edu$HS <- as.numeric(edu$HS)
edu$bachelors <- as.numeric(edu$bachelors)
edu$FIPS <- as.character(edu$FIPS)

# EMPLOYMENT

# fix fips to include state code
employment$code <- paste(employment$code_2, employment$code_3)
employment$code <- gsub(" ", "", employment$code)
employment$code <- str_remove(employment$code, "^0+")
# keep only fips, unemployment percent
employment <- dplyr::select(employment, code, percent)
employment <- rename(employment, FIPS = code)
employment <- rename(employment, unemp_percent = percent)
employment$unemp_percent <- as.numeric(employment$unemp_percent)

#Race Data 
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


#Create Main Dataframe 
maindf <- inner_join(mortality_rate_df, pov, by = "FIPS")
maindf <- inner_join(maindf, employment, by = "FIPS")
maindf <- inner_join(maindf, edu, by = "FIPS")
maindf <- inner_join(maindf, latlong, by = "FIPS")
maindf$poverty_percent_all_ages <- as.integer(maindf$poverty_percent_all_ages)
maindf <- inner_join(maindf, race, by = "FIPS")

# Working w/ data
# Create Quartiles 
#Poverty Quartiles
quantile(maindf$poverty_percent_all_ages)
#Q1 : 9.9, Q2 : 12.8, Q3 : 16.6, Q4 : 43.9

maindf$pov_quartile <- as.factor(ifelse(maindf$poverty_percent_all_ages <= 9.9, '1', 
                                     ifelse(maindf$poverty_percent_all_ages <= 12.8, '2', 
                                            ifelse(maindf$poverty_percent_all_ages <= 16.6, '3', 
                                                   ifelse(maindf$poverty_percent_all_ages <= 43.9, '4')))))
# Median Household Income Quartiles
#find quantiles 
quantile(maindf$median_household_income)
maindf$medhh_quartile <- as.factor(ifelse(maindf$median_household_income <= 47722, '1', 
                                              ifelse(maindf$median_household_income <= 55055, '2', 
                                                     ifelse(maindf$median_household_income <= 63963, '3',
                                                            ifelse(maindf$median_household_income <= 160305, '4')))))
# Bachelor's Degree Quartiles
#split into quantiles? 
quantile(maindf$bachelors)
maindf$bachelors_quartile <- as.factor(ifelse(maindf$bachelors <= 15.3, '1',
                                                  ifelse(maindf$bachelors <= 19.5, '2', 
                                                         ifelse(maindf$bachelors <= 26, '3', 
                                                                ifelse(maindf$bachelors <= 77.6, '4'))))) 
# Mortality Rate Quartiles
maindf$ratio_quartile <- as.factor(ifelse(maindf$ratio <= 0.010225385, '1',
                                              ifelse(maindf$ratio <= 0.013472447, '2', 
                                                     ifelse(maindf$ratio <= 0.017375413, '3', 
                                                            ifelse(maindf$ratio <= 0.05, '4'))))) 
#Find white quartile
quantile(maindf$White_perc)
#create new column with white quartiles 
maindf$white_quartile <- as.factor(ifelse(maindf$White_perc <= 0.79520388, '1', 
                                        ifelse(maindf$White_perc <= .91262314, '2', 
                                               ifelse(maindf$White_perc <= 0.95411157, '3', 
                                                      ifelse(maindf$White_perc <= 1, '4')))))
#Find black quartiles
quantile(maindf$Black_perc)
maindf$black_quartile <- as.factor(ifelse(maindf$Black_perc <= 0.008927714, '1', 
                                        ifelse(maindf$Black_perc <= 0.025427597, '2', 
                                               ifelse(maindf$Black_perc <= 0.108602869, '3', 
                                                      ifelse(maindf$Black_perc <= 1, '4')))))
#Find hispanic quartiles
quantile(maindf$Hispanic_perc)
maindf$hispanic_quartile <- as.factor(ifelse(maindf$Hispanic_perc <= 0.024699270, '1', 
                                           ifelse(maindf$Hispanic_perc <= 0.044783246, '2', 
                                                  ifelse(maindf$Hispanic_perc <= 0.101442643, '3', 
                                                         ifelse(maindf$Hispanic_perc <= 1, '4')))))
#Find asian quartiles 
quantile(maindf$Asian_perc)
maindf$asian_quartile <- as.factor(ifelse(maindf$Asian_perc <= 0.004774536, '1', 
                                        ifelse(maindf$Asian_perc <= 0.007524692, '2', 
                                               ifelse(maindf$Asian_perc <= 0.014507827, '3', 
                                                      ifelse(maindf$Asian_perc <= 1, '4')))))



# Unlist ratio for ggplot
maindf$ratio <- unlist(maindf$ratio)

#Find Mortality Rate Each Day
# get rid of unnecessary columns from deaths and cases
deaths <- deaths[, -c(1:4, 6, 8:54)]
cases <- cases[, -c(1:4, 6, 8:53)]

deaths <- na.omit(deaths)
cases <- na.omit(cases)

mr_trend <- data.frame(deaths$FIPS)
for(i in 3:ncol(cases)){
  x <- deaths[[i]]
  y <- cases[[i]]
  new <- x/y
  mr_trend[, ncol(mr_trend) + 1] <- new
  colnames(mr_trend)[ncol(mr_trend)] <- paste0("new", i)
}

mr_trend[is.na(mr_trend)] <- 0

names(mr_trend)[names(mr_trend) == "deaths.FIPS"] <- "FIPS"

# Create Infection Rate Variable 
population <- race[, c(1,2)]
population$FIPS <- as.numeric(population$FIPS)
oct_cases <- cases[, c(1,959)]
inf_rate <- left_join(oct_cases, population, by = "FIPS")
inf_rate <- na.omit(inf_rate)
inf_rate$infection_ratio <- inf_rate$X10.16.22 / inf_rate$TOT_POP
inf_rate <- filter(inf_rate, infection_ratio <= 1)
inf_rate$FIPS <- as.character(inf_rate$FIPS)

# Add infection ratio to main data frame
maindf <- inner_join(maindf, inf_rate, by = c("FIPS", "TOT_POP"))
maindf <- na.omit(maindf)

# Find Infection Rate each day 
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
ir_trend[is.na(ir_trend)] <- 0

names(ir_trend)[names(ir_trend) == "temp.FIPS"] <- "FIPS"
#change FIPS to character for mr_trend 
ir_trend$FIPS <- as.character(ir_trend$FIPS)
ir_trend <- ir_trend[, -c(1913)]

# create infection rate quartiles
maindf$ir_quartile <- as.factor(ifelse(maindf$infection_ratio <= 0.2464198, '1', 
                                       ifelse(maindf$infection_ratio <= 0.2843289, '2', 
                                              ifelse(maindf$infection_ratio <= 0.3223086, '3',
                                                     ifelse(maindf$infection_ratio <= .8569386, '4')))))

# POVERTY ANALYSIS : MORTALITY RATE

#overall poverty distribution w/in US
ggplot(data = maindf) +
  geom_density(mapping = aes(x = poverty_percent_all_ages))

#graph density of mortality rate by quartile
ggplot(data = maindf, aes(ratio, color = pov_quartile)) + 
  geom_density() +
  labs(x = "Mortality Rate", y = "Density", 
       title = "Mortality Rate Density Curve", 
       color = "Poverty Percent Quartile")


#subset poverty data to find mean of each quartile 
pov_1 <- maindf[maindf$pov_quartile == "1", ]
pov_quartile_1mean <- mean(pov_1$ratio)
# mean = 0.01083488
pov_2 <- maindf[maindf$pov_quartile == "2", ]
pov_quartile_2mean <- mean(pov_2$ratio)
# mean = 0.01387041
pov_3 <- maindf[maindf$pov_quartile == "3", ]
pov_quartile_3mean <- mean(pov_3$ratio)
# mean = 0.01561862
pov_4 <- maindf[maindf$pov_quartile == "4", ]
pov_quartile_4mean <- mean(pov_4$ratio)
# mean = 0.01701428

#find median of each poverty quartile 
median(pov_1$ratio)
median(pov_2$ratio)  
median(pov_3$ratio)
median(pov_4$ratio)
median(maindf$ratio)

#Working w/ median household income data 
#change median household income to numeric vector 
maindf$median_household_income <- as.numeric(gsub(",", "", 
                                                  maindf$median_household_income))

#distribution of med hh income in US
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = median_household_income))


#plot mortality rate by med hh income quartiles
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = ratio, color = medhh_quartile)) +
  labs(x = "Mortality Rate", y = "Density", 
       title = "Mortality Rate Density", 
       color = "Med. HH Income Quartile")

#find avg mortality rate among median HH income quartiles 
mean_medhh1 <- mean(maindf$ratio[maindf$medhh_quartile == '1'])
# mean = 0.01789561
mean_medhh2 <- mean(maindf$ratio[maindf$medhh_quartile == '2'])
# mean = 0.01573182
mean_medhh3 <- mean(maindf$ratio[maindf$medhh_quartile == '3'])
# mean = 0.01331643
mean_medhh4 <- mean(maindf$ratio[maindf$medhh_quartile == '4'])
# mean = 0.01032676

# Create Dataframes for Trendlines
mr_trend$FIPS <- as.character(mr_trend$FIPS)
mr_trend_pov <- inner_join(maindf, mr_trend, by = "FIPS")
mr_trend_pov <- na.omit(mr_trend_pov)

#aggregate df to find avg mortality rate each day w/in the quartiles
mr_trend_agg <- aggregate(mr_trend_pov, by = list(mr_trend_pov$pov_quartile), 
                          FUN = mean)
is.na(mr_trend_agg) <- sapply(mr_trend_agg, is.infinite)
mr_trend_agg[is.na(mr_trend_agg)] <- 0

transpose_pov <- t(mr_trend_agg)
#remove everything except Average mortality rates
transpose_pov <- transpose_pov[-c(1:29), ]
colnames(transpose_pov) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")
# Import Dates used for Trendlines
dates <- read.csv("C:/Users/carly/OneDrive/Documents/R/Data/Dates.csv", 
                  header = FALSE)
colnames(dates) <- c("Date")
bind_pov <- cbind(dates, transpose_pov)
bind_pov$Date <- as.Date(bind_pov$Date)
bind_pov$Quartile_1 <- as.numeric(bind_pov$Quartile_1)
bind_pov$Quartile_2 <- as.numeric(bind_pov$Quartile_2)
bind_pov$Quartile_3 <- as.numeric(bind_pov$Quartile_3)
bind_pov$Quartile_4 <- as.numeric(bind_pov$Quartile_4)
ggplot(data = bind_pov) +
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

ggplot(data = maindf) +
  geom_boxplot(mapping = aes(x = pov_quartile, y = ratio, color = pov_quartile)) +
  labs(x = "Poverty Percent Quartile", y = "Mortality Rate", 
       title = "Mortality Rate Distribution by Poverty Quartile", 
       color = "Poverty Percent Quartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("mortality rate distribution by POV BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

# POVERTY ANALYSIS : INFECTION RATE 

#infection rate trend by POV Quartile 
ir_trend_pov <- left_join(maindf, ir_trend, by = "FIPS")
ir_trend_pov <- na.omit(ir_trend_pov)

#aggregate df to find avg mortality rate each day w/in the poverty quartiles
ir_trend_agg <- aggregate(ir_trend_pov, by = list(ir_trend_pov$pov_quartile), FUN = mean)
is.na(ir_trend_agg) <- sapply(ir_trend_agg, is.infinite)
ir_trend_agg[is.na(ir_trend_agg)] <- 0
ir_trend_agg <- ir_trend_agg[, -c(1:29)]

transpose_ir <- t(ir_trend_agg)
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
ggplot(data = maindf) +
  geom_density(mapping = aes(x = infection_ratio, color = pov_quartile)) +
  labs(x = "Infection Rate", y = "Density", title = "Infection Rate Density", 
       color = "Poverty\n Quartile") +
  theme(plot.title = element_text(hjust = 0.5))


#Box plot for Infection rate 
ggplot(data = maindf) +
  geom_boxplot(mapping = aes(x = pov_quartile, y = infection_ratio, 
                             color = pov_quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "Poverty Quartile", y = "Infection Rate", 
       title = "Infection Rate by Poverty Quartile", color = "Poverty\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by POV oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)



# EDUCATION ANALYSIS : MORTALITY RATE

ggplot(data = maindf, aes(x = no_HS, y = ratio)) +
  geom_point() +
  geom_jitter(alpha = 0.5)

# relationship between mortality rate & percentage of county that completed HS
ggplot(data = maindf, aes(x = HS, y = ratio)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "% of County that Completed High School", y = "Mortality Rate", 
       title = "Mortality Rate vs Percent of County that Completed High school") +
  theme(plot.title = element_text(hjust = 0.5))



# Relationship between mortality rate & percentage of county with a bachelor's
ggplot(data = maindf) +
  geom_point(mapping = aes(x = bachelors, y = ratio))

# Barplot w/ Mortality rate on x-axis; bars broken into Bachelor's quartiles
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = ratio_quartile, y = ..count.., fill = bachelors_quartile)) +
  labs(x = "Mortality Rate Quartile", y = "Count", title = "Mortality Rate by Bachelor's Quartile", 
       fill = "Bachelor's Degree\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("mr quartile by BD Bar.png", 
       plot = last_plot(), width = 5.5, height = 4)

# Distribution of mortality rate by bachelor's quartiles
ggplot(data = maindf) +
  geom_density(mapping = aes(x = ratio, color = bachelors_quartile)) +
  labs(x = "Mortality Rate", y = "Density", title = "Mortality Rate Density", 
       color = "Bachelor Degree\n Quartile") +
  theme(plot.title = element_text(hjust = 0.5))
mean(maindf$ratio)

# POSSIBLY REMOVE (NOT USEFUL?) !!!
ggplot(data = maindf) +
  geom_histogram(mapping = aes(x = ratio, fill = bachelors_quartile))

# Distribution of Mortality Rate by Bachelor's quartile
ggplot(data = maindf) +
  geom_boxplot(mapping = aes(x = bachelors_quartile, y = ratio, color = bachelors_quartile)) +
  labs(x = "Bachelor Degree Quartile", y = "Mortality Rate", 
       title = "Mortality Rate Distribution by Bachelor's Degree Quartile", 
       color = "Bachelor's Degree\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("mortality rate distribution by BD BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

# trend by bachelor's quartile

#aggregate df to find avg mortality rate each day w/in the quartiles
mr_trend_bachelors <- left_join(maindf, mr_trend, by = "FIPS")
mr_trend_bachelors <- na.omit(mr_trend_bachelors)
mr_trend_bachelors <- aggregate(mr_trend_bachelors, by = list(mr_trend_bachelors$bachelors_quartile), FUN = mean)
is.na(mr_trend_bachelors) <- sapply(mr_trend_bachelors, is.infinite)
mr_trend_bachelors[is.na(mr_trend_bachelors)] <- 0
mr_trend_bachelors <- mr_trend_bachelors[, -c(1:29)]

transpose_bachelors <- t(mr_trend_bachelors)
colnames(transpose_bachelors) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

bind_bachelors <- cbind(dates, transpose_bachelors)
bind_bachelors$Date <- as.Date(bind_bachelors$Date)
bind_bachelors$Quartile_1 <- as.numeric(bind_bachelors$Quartile_1)
bind_bachelors$Quartile_2 <- as.numeric(bind_bachelors$Quartile_2)
bind_bachelors$Quartile_3 <- as.numeric(bind_bachelors$Quartile_3)
bind_bachelors$Quartile_4 <- as.numeric(bind_bachelors$Quartile_4)



# bachelor's quartile trendlines
ggplot(data = bind_bachelors) +
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


#median mortality rate among bachelor's quartiles 
bachelors_one_med <- median(maindf$ratio[maindf$bachelors_quartile == "1"])

bachelors_two_med <- median(maindf$ratio[maindf$bachelors_quartile == "2"])

bachelors_three_med <- median(maindf$ratio[maindf$bachelors_quartile == "3"])

bachelors_four_med <- median(maindf$ratio[maindf$bachelors_quartile == "4"])


# find mean mortality rate of each bachelor's quartile
bachelors_one_avg <- mean(maindf$ratio[maindf$bachelors_quartile == "1"])

bachelors_two_avg <- mean(maindf$ratio[maindf$bachelors_quartile == "2"])

bachelors_three_avg <- mean(maindf$ratio[maindf$bachelors_quartile == "3"])

bachelors_four_avg <- mean(maindf$ratio[maindf$bachelors_quartile == "4"])


#subset education quartiles 
bachelors_mr1 <- maindf[maindf$bachelors_quartile == "1", ]
bachelors_mr2 <- maindf[maindf$bachelors_quartile == "2", ]
bachelors_mr3 <- maindf[maindf$bachelors_quartile == "3", ]
bachelors_mr4 <- maindf[maindf$bachelors_quartile == "4", ]


#calculate confidence interval for education 
se <- sd(maindf$ratio) / sqrt(nrow(bachelors_mr4))
sample_mean <- mean(maindf$ratio)
lower_bound <- sample_mean - 1.96*se
upper_bound <- sample_mean + 1.96*se
bachelors_four_avg
# lower bound is 1.38
# Upper bound is 1.477
# mean is 1.03
# find z score 
pnorm(0.01030835, mean = sample_mean, sd = se)
z_bachelors4 <- (mean(maindf$ratio) - bachelors_four_avg) / se


# EDUCATION ANALYSIS : INFECTION RATE
ggplot(data = maindf) +
  geom_boxplot(mapping = aes(x = bachelors_quartile, y = infection_ratio, 
                             color = bachelors_quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "Bachelor's Quartile", y = "Infection Rate", 
       title = "Infection Rate by Bachelor's Quartile", color = "Bachelor's\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by BD oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

# Infection rate distribution for bachelor's quartiles
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = bachelors_quartile, y = ..count.., fill = ir_quartile)) +
  labs(x = "Infection Rate Quartile", y = "Count", title = "Bachelor's Degree by Infection Rate", 
       fill = "Infection Rate\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("bachelors quartile by ir Bar.png", 
       plot = last_plot(), width = 5.5, height = 4)


#RACE ANALYSIS : MORTALITY RATE

# Create Dataframes for Race Quartile Trendlines
mr_trend_race <- left_join(maindf, mr_trend, by = "FIPS")
mr_trend_race <- mr_trend_race[, -c(2:21)]
mr_white_agg <- aggregate(mr_trend_race, by = list(mr_trend_race$white_quartile), FUN = mean)
mr_white_agg <- mr_white_agg[, -c(1:9)]
is.na(mr_white_agg) <- sapply(mr_white_agg, is.infinite)
mr_white_agg[is.na(mr_white_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_white <- t(mr_white_agg)
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
mr_black_agg <- aggregate(mr_trend_race, by = list(mr_trend_race$black_quartile), FUN = mean)
mr_black_agg <- mr_black_agg[, -c(1:9)]
is.na(mr_black_agg) <- sapply(mr_black_agg, is.infinite)
mr_black_agg[is.na(mr_black_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose_black <- t(mr_black_agg)
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
mr_hispanic_agg <- mr_hispanic_agg[, -c(1:9)]
is.na(mr_hispanic_agg) <- sapply(mr_hispanic_agg, is.infinite)
mr_hispanic_agg[is.na(mr_hispanic_agg)] <- 0

transpose_hispanic <- t(mr_hispanic_agg)
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
mr_asian_agg <- mr_asian_agg[, -c(1:9)]
is.na(mr_asian_agg) <- sapply(mr_asian_agg, is.infinite)
mr_asian_agg[is.na(mr_asian_agg)] <- 0

transpose_asian <- t(mr_asian_agg)
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

# RACE ANALYSIS : INFECTION RATE 
ggplot(data = maindf) +
  geom_boxplot(mapping = aes(x = white_quartile, y = infection_ratio, 
                             color = white_quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "White Quartile", y = "Infection Rate", 
       title = "Infection Rate by White Quartile", color = "White\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by white oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

ggplot(data = maindf) +
  geom_boxplot(mapping = aes(x = black_quartile, y = infection_ratio, 
                             color = black_quartile)) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = "Black Quartile", y = "Infection Rate", 
       title = "Infection Rate by Black Quartile", color = "Black\nQuartile") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size=10))
ggsave("infection rate distribution by black oct BP.png", 
       plot = last_plot(), width = 5.5, height = 4)

#MAPS 
set.seed(1)    # for reproducible example
map.county <- map_data('county')
counties   <- unique(map.county[,5:6])
pov_map <- data.frame(state_names=counties$region, 
                      county_names=counties$subregion, 
                      pov_percentage = runif(nrow(counties), min=0, max=100))

map.county <- data.table(map_data('county'))
setkey(map.county,region,subregion)
pov_map <- data.table(pov_map)
setkey(pov_map,state_names,county_names)
map.df <- map.county[pov_map]

high_pov <- filter(maindf, as.integer(pov_quartile) >= 3)
high_pov <- dplyr::select(high_pov, FIPS, Lat, long)
high_pov <- filter(high_pov, str_starts(high_pov$FIPS, '15') != TRUE)
high_pov <- filter(high_pov, str_starts(high_pov$FIPS, '02') != TRUE)
high_pov$Lat <- unlist(high_pov$Lat)
high_pov$long <- unlist(high_pov$long)

high_ratio <- filter(maindf, as.integer(ratio_quartile) >= 3)
high_ratio <- dplyr::select(high_ratio, FIPS, Lat, long, ratio_quartile)
high_ratio <- filter(high_ratio, str_starts(high_ratio$FIPS, '15') != TRUE)
high_ratio <- filter(high_ratio, str_starts(high_ratio$FIPS, '02') != TRUE)
high_ratio$Lat <- unlist(high_ratio$Lat)
high_ratio$long <- unlist(high_ratio$long)

low_pov <- filter(maindf, as.integer(pov_quartile) < 3)
low_pov <- dplyr::select(low_pov, FIPS, Lat, long)
low_pov <- filter(low_pov, str_starts(low_pov$FIPS, '15') != TRUE)
low_pov <- filter(low_pov, str_starts(low_pov$FIPS, '02') != TRUE)
low_pov$Lat <- unlist(low_pov$Lat)
low_pov$long <- unlist(low_pov$long)

low_ratio <- filter(maindf, as.integer(ratio_quartile) < 3)
low_ratio <- dplyr::select(low_ratio, FIPS, Lat, long, ratio_quartile)
low_ratio <- filter(low_ratio, str_starts(low_ratio$FIPS, '15') != TRUE)
low_ratio <- filter(low_ratio, str_starts(low_ratio$FIPS, '02') != TRUE)
low_ratio$Lat <- unlist(low_ratio$Lat)
low_ratio$long <- unlist(low_ratio$long)

# LOW POVERTY, HIGH MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = low_pov, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "white") +
  geom_point(data = high_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "red") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "High mortality rate, Low poverty levels") 

# LOW POVERTY, LOW MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = low_pov, mapping = aes(x = long, y = Lat), 
             alpha = .7, color = "white") +
  geom_point(data = low_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "green") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "Low mortality rate, Low poverty levels") 

# HIGH POVERTY, HIGH MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = high_pov, mapping = aes(x = long, y = Lat), 
             alpha = .7, color = "white") +
  geom_point(data = high_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "red") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "High mortality rate, high poverty levels") 

# HIGH POVERTY, LOW MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = high_pov, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "white") +
  geom_point(data = low_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "green") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "Low mortality rate, high poverty levels") 

# RACE QUARTILES 
# WHITE
high_white <- filter(maindf, as.integer(white_quartile) >= 3)
high_white <- dplyr::select(high_white, FIPS, Lat, long)
high_white <- filter(high_white, str_starts(high_white$FIPS, '15') != TRUE)
high_white <- filter(high_white, str_starts(high_white$FIPS, '02') != TRUE)
high_white$Lat <- unlist(high_white$Lat)
high_white$long <- unlist(high_white$long)

# BlACK
high_black <- filter(maindf, as.integer(black_quartile) >= 3)
high_black <- dplyr::select(high_black, FIPS, Lat, long)
high_black <- filter(high_black, str_starts(high_black$FIPS, '15') != TRUE)
high_black <- filter(high_black, str_starts(high_black$FIPS, '02') != TRUE)
high_black$Lat <- unlist(high_black$Lat)
high_black$long <- unlist(high_black$long)

# HIGH WHITE, LOW MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = high_white, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "white") +
  geom_point(data = low_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "green") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "Low mortality rate, high white percentage") 

# HIGH WHITE, HIGH MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = high_white, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "white") +
  geom_point(data = high_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "red") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "High mortality rate, high white percentage") 

# HIGH BLACK, LOW MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = high_black, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "white") +
  geom_point(data = low_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "green") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "Low mortality rate, high black percentage") 

# HIGH BLACK, HIGH MORTALITY
ggplot() + 
  geom_polygon(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage))+
  coord_map(data = map.df, aes(x=long, y=lat, group=group, fill=pov_percentage)) +
  geom_point(data = high_black, mapping = aes(x = long, y = Lat), 
             alpha = .7, color = "white") +
  geom_point(data = high_ratio, mapping = aes(x = long, y = Lat), 
             alpha = .5, color = "red") +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 50)) +
  labs (title = "High mortality rate, high black percentage") 


# CORRELATION COEFFICIENTS?
# race all ages
white_cor <- cor(maindf$White_perc, maindf$ratio, method = "pearson") # -0.01587196
black_cor <- cor(maindf$Black_perc, maindf$ratio, method = "pearson") # 0.1183769
asian_cor <- cor(maindf$Asian_perc, maindf$ratio, method = "pearson") # -0.2737742
hispanic_cor <- cor(maindf$Hispanic_perc, maindf$ratio, method = "pearson") # 0.03555343
# note: asian and white are neg correlated, hispanic and black pos correlated w mortality rate
# education
bachelors_cor <- cor(maindf$bachelors, maindf$ratio, method = "pearson") # -0.4280797
hs_cor <- cor(as.integer(maindf$HS), maindf$ratio, method = "pearson") # 0.3498861
nohs_cor <- cor(as.integer(maindf$no_HS), maindf$ratio, method = "pearson") # 0.3134558
# note: bachelors only one negatively correlated and has strongest correlation of anything
# poverty
pov_cor <- cor(maindf$poverty_percent_all_ages, maindf$ratio, method = "pearson") # 0.3453266
unemp_cor <- cor(maindf$unemp_percent, maindf$ratio, method = "pearson") # 0.08287031
hhi_cor <- cor(maindf$median_household_income, maindf$ratio, method = "pearson") # -0.4422267
# unemp probably irrelevant but can be used as example

correlations <- data.frame(white_cor, black_cor, asian_cor, hispanic_cor, bachelors_cor,
                           hs_cor, nohs_cor, pov_cor, unemp_cor, hhi_cor)
X <- names(correlations)
Y <- names(correlations)
correlations <- t(correlations)
correlations <- data.frame(correlations)

# HEAT MAP OF CORRELATIONS
# blue = pos corr
# red = neg corr
maindf$no_HS <- as.numeric(maindf$no_HS)
maindf$HS <- as.numeric(maindf$HS)
groups <- dplyr::select(maindf, ratio, poverty_percent_all_ages, median_household_income, unemp_percent,
                 White_perc, Black_perc, Asian_perc, Hispanic_perc, 
                 bachelors, no_HS, HS)
groups_corrs <- cor(groups, method = "spearman")
groups_corrp <- cor(groups, method = "pearson")
corrplot(abs(groups_corrp), method = "color", tl.col = "black", tl.cex = .75, is.corr = FALSE,
         cl.lim=c(0,.5), col=colorRampPalette(c("steelblue", "lightblue1"))(200))
corrplot(abs(groups_corrs), method = "color", tl.col = "black", tl.cex = .75, is.corr = FALSE,
         cl.lim=c(0,1), col=colorRampPalette(c("steelblue", "lightblue1"))(200))
