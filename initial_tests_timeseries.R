library(ggplot2)
library(janitor)
library(dplyr)
library(stringr)
library(corrplot)

### READ IN DATASETS ... ###

# CASES
cases <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/us_timeseries_confirmed.txt", 
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)

# DEATHS
deaths <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/us_timeseries_deaths.txt", 
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)

# ALL COUNTRIES LOL merge with ours we just want the lat/long data
countries <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/country_daily.txt", 
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)

# MORTALITY RATE DF
mortality_rate_df <- cases[c(1:11, length(cases))]
mortality_rate_df$cases <- cases[c(length(cases))]
mortality_rate_df$deaths <- deaths[c(length(cases))]
mortality_rate_df$ratio <- mortality_rate_df$deaths / mortality_rate_df$cases
mortality_rate_df$ratio <- as.vector(mortality_rate_df$ratio$X10.15.22)

# RACE
race <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/race.csv", 
                  fill = TRUE)

# EMPLOYMENT
employment <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/laucnty21 (1).csv",
                 fill = TRUE)

# UID reference
uidref <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/uidlookup.txt",
                     quote = "\"", header = TRUE, sep = ",", fill = TRUE)
uidref <- uidref[which(uidref$iso3 == "USA"),]
uidref <- uidref[-c(2:177),]

# POVERTY 
pov <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/poverty_data.csv", header = FALSE)

# EDUCATION
edu <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/Education.csv",
                fill = TRUE)


#####################
### CLEANING ... ####
#####################

# MORTALITY RATE 
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

# LAT AND LONG
countries <- countries[676:3954,]
latlong <- select(countries, FIPS, Lat, Long_)
latlong <- rename(countries, long = Long_)
latlong$FIPS <- as.character(latlong$FIPS)
latlong <- select(latlong, FIPS, Lat, long)

# POVERTY
# clean row names
pov <- row_to_names(pov, 4, remove_rows_above = TRUE)
pov <- clean_names(pov)
pov <- pov[-1,]
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
# clean row names
employment <- row_to_names(employment, 4, remove_rows_above = TRUE)
employment <- clean_names(employment)
employment <- employment[-1,]
# fix fips to include state code
employment$code <- paste(employment$code_2, employment$code_3)
employment$code <- gsub(" ", "", employment$code)
employment$code <- str_remove(employment$code, "^0+")
# keep only fips, unemployment percent
employment <- select(employment, code, percent)
employment <- rename(employment, FIPS = code)
employment <- rename(employment, unemp_percent = percent)

# RACE 
# filter out useful info
race <- race[race$YEAR == 12, ]
race <- select(race, SUMLEV, STATE, COUNTY, CTYNAME, AGEGRP, TOT_POP,
               WA_MALE, WA_FEMALE,
               BA_MALE, BA_FEMALE,
               IA_MALE, IA_FEMALE,
               AA_MALE, AA_FEMALE,
               NA_MALE, NA_FEMALE,
               H_MALE, H_FEMALE)
# create total race population value
race$White <- race$WA_MALE + race$WA_FEMALE
race$Black <- race$BA_MALE + race$BA_FEMALE
race$Native <- race$IA_MALE + race$IA_FEMALE
race$Asian <- race$AA_MALE + race$AA_FEMALE
race$Hawaiian <- race$NA_MALE + race$NA_FEMALE
race$Hispanic <- race$H_MALE + race$H_FEMALE
# keep the total race populations; get rid of populations by gender
race <- select(race, STATE, COUNTY, CTYNAME, AGEGRP, TOT_POP,
               White,
               Black,
               Native,
               Asian,
               Hawaiian,
               Hispanic)
# create total race percentages
race$White_perc <- race$White / race$TOT_POP
race$Black_perc <- race$Black / race$TOT_POP
race$Native_perc <- race$Native / race$TOT_POP
race$Asian_perc <- race$Asian / race$TOT_POP
race$Hawaiian_perc <- race$Hawaiian / race$TOT_POP
race$Hispanic_perc <- race$Hispanic / race$TOT_POP
race <- select(race, STATE, COUNTY, CTYNAME, AGEGRP, TOT_POP,
               White_perc,
               Black_perc,
               Native_perc,
               Asian_perc,
               Hawaiian_perc,
               Hispanic_perc)
# create fips
race$COUNTY <- str_pad(race$COUNTY, width = 3, pad = "0")
race$FIPS <- paste(as.character(race$STATE), race$COUNTY)
race$FIPS <- gsub(" ", "", race$FIPS)
race <- select(race, AGEGRP, TOT_POP,
               White_perc,
               Black_perc,
               Native_perc,
               Asian_perc,
               Hawaiian_perc,
               Hispanic_perc, FIPS)
# only keep the TOTAL age groups
race_allgroups$AGEGRP <- as.factor(race_allgroups$AGEGRP)
race <- race[race$AGEGRP == 0, ]
race <- select(race, FIPS, TOT_POP,
               White_perc,
               Black_perc,
               Native_perc,
               Asian_perc,
               Hawaiian_perc,
               Hispanic_perc)


# EDUCATION
# clean row names
edu <- row_to_names(edu, 4, remove_rows_above = TRUE)
edu <- clean_names(edu)
edu <- edu[-1,]
# add fips 
edu$FIPS <- sub("^0+", "", edu$fips_code) 
# filter out relevant info and rename it
edu <- select(edu, FIPS, percent_of_adults_with_less_than_a_high_school_diploma_2015_19,
              percent_of_adults_with_a_high_school_diploma_only_2015_19,
              percent_of_adults_with_a_bachelors_degree_or_higher_2015_19)
edu <- rename(edu, no_hs_diploma = percent_of_adults_with_less_than_a_high_school_diploma_2015_19,
              hs_diploma = percent_of_adults_with_a_high_school_diploma_only_2015_19,
              bachelors = percent_of_adults_with_a_bachelors_degree_or_higher_2015_19)
edu$bachelors <- as.integer(edu$bachelors)

# JOIN MAIN DATA FRAME 
maindf <- inner_join(mortality_rate_df, pov)
maindf <- inner_join(maindf, employment)
maindf <- inner_join(maindf, edu)
maindf <- inner_join(maindf, latlong)
maindf$poverty_percent_all_ages <- as.integer(maindf$poverty_percent_all_ages)
maindf <- inner_join(maindf, race)

###################
### GRAPHS #######
##################

### QUARTILES ###

# POVERTY QUARTILES
maindf$poverty_percent_all_ages <- as.numeric(as.character(maindf$poverty_percent_all_ages))
# quantile(maindf$poverty_percent_all_ages)
maindf$pov_quartile <- as.factor(ifelse(maindf$poverty_percent_all_ages <= 9.9, '1', 
                                 ifelse(maindf$poverty_percent_all_ages <= 12.8, '2', 
                                        ifelse(maindf$poverty_percent_all_ages <= 16.6, '3', 
                                               ifelse(maindf$poverty_percent_all_ages <= 43.9, '4')))))
# 1 = lowest perc. pov, 4 = highest perc. pov
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

# UNEMPLOYMENT QUARTILES
maindf$unemp_percent <- as.numeric(as.character(maindf$unemp_percent))
quantile(maindf$unemp_percent)
maindf$unemp_quartile <- as.factor(ifelse(maindf$unemp_percent <= 3.5, '1', 
                                          ifelse(maindf$unemp_percent <= 4.4, '2', 
                                                 ifelse(maindf$unemp_percent <= 5.5, '3', 
                                                        ifelse(maindf$unemp_percent <= 19.9, '4')))))
#find avg mortality rate among different unemployment rate groups 
mean_unemp1 <- mean(maindf$ratio[maindf$unemp_quartile == '1'])
# mean = 0.01354782
mean_unemp2 <- mean(maindf$ratio[maindf$unemp_quartile == '2'])
# mean = 0.01421827
mean_unemp3 <- mean(maindf$ratio[maindf$unemp_quartile == '3'])
# mean = 0.01437989
mean_unemp4 <- mean(maindf$ratio[maindf$unemp_quartile == '4'])
# mean = 0.01519225

# EDUCATION QUARTILES
maindf$bachelors_quartile <- as.factor(ifelse(maindf$bachelors <= 15, '1',
                                              ifelse(maindf$bachelors <= 19, '2', 
                                                     ifelse(maindf$bachelors <= 26, '3', 
                                                            ifelse(maindf$bachelors <= 77, '4')))))
#find avg mortality rate among different education groups 
mean_bc1 <- mean(maindf$ratio[maindf$bachelors_quartile == '1'])
# mean = 0.01698056
mean_bc2 <- mean(maindf$ratio[maindf$bachelors_quartile == '2'])
# mean = 0.01561655
mean_bc3 <- mean(maindf$ratio[maindf$bachelors_quartile == '3'])
# mean = 0.01391756
mean_bc4 <- mean(maindf$ratio[maindf$bachelors_quartile == '4'])
# mean = 0.01004702

# MORTALITY RATIO QUARTILES
maindf$ratio_quartile <- as.factor(ifelse(maindf$ratio <= 0.010225385, '1',
                                          ifelse(maindf$ratio <= 0.013472447, '2', 
                                                 ifelse(maindf$ratio <= 0.017375413, '3', 
                                                        ifelse(maindf$ratio <= 0.05, '4')))))

# WHITE QUARTILES
maindf$white_quartile <- as.factor(ifelse(maindf$White_perc <= 0.79436296, '1',
                                          ifelse(maindf$White_perc <= 0.91197499, '2', 
                                                 ifelse(maindf$White_perc <= 0.95398971, '3', 
                                                        ifelse(maindf$White_perc <= 1, '4')))))
mean_white1 <- mean(maindf$ratio[maindf$white_quartile == '1'])
# mean = 0.01455018
mean_white2 <- mean(maindf$ratio[maindf$white_quartile == '2'])
# mean = 0.01337807
mean_white3 <- mean(maindf$ratio[maindf$white_quartile == '3'])
# mean = 0.01429719
mean_white4 <- mean(maindf$ratio[maindf$white_quartile == '4'])
# mean = 0.01506888

# BLACK QUARTILES
maindf$black_quartile <- as.factor(ifelse(maindf$Black_perc <= 0.009089983, '1',
                                          ifelse(maindf$Black_perc <= 0.026571664, '2', 
                                                 ifelse(maindf$Black_perc <= 0.865932354, '3', 
                                                        ifelse(maindf$Black_perc <= 1, '4')))))
mean_black1 <- mean(maindf$ratio[maindf$black_quartile == '1'])
# mean = 0.01497474
mean_black2 <- mean(maindf$ratio[maindf$black_quartile == '2'])
# mean = 0.01360205
mean_black3 <- mean(maindf$ratio[maindf$black_quartile == '3'])
# mean = 0.01435565
mean_black4 <- mean(maindf$ratio[maindf$black_quartile == '4'])
# mean = 0.01941748 SIGNFICANT JUMP !
# NOTE: we should probably use quartiles based on location like per state for these
# instead of overall cuz some states r VERY different from others and it throws it off

# ASIAN QUARTILES
maindf$asian_quartile <- as.factor(ifelse(maindf$Asian_perc <= 0.00477788, '1',
                                          ifelse(maindf$Asian_perc <= 0.00751634, '2', 
                                                 ifelse(maindf$Asian_perc <= 0.01450692, '3', 
                                                        ifelse(maindf$Asian_perc <= 0.5, '4')))))
mean_asian1 <- mean(maindf$ratio[maindf$asian_quartile == '1'])
# mean = 0.01641049
mean_asian2 <- mean(maindf$ratio[maindf$asian_quartile == '2'])
# mean = 0.01552386
mean_asian3 <- mean(maindf$ratio[maindf$asian_quartile == '3'])
# mean = 0.01473168
mean_asian4 <- mean(maindf$ratio[maindf$asian_quartile == '4'])
# mean = 0.01063004

# HISPANIC QUARTILES
maindf$hispanic_quartile <- as.factor(ifelse(maindf$Hispanic_perc <= 0.024542348, '1',
                                          ifelse(maindf$Hispanic_perc <= 0.024542348, '2', 
                                                 ifelse(maindf$Hispanic_perc <= 0.101297288, '3', 
                                                        ifelse(maindf$Hispanic_perc <= 1, '4')))))

### GRAPHS ###

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
# GOOD - how to interpret?
# density of mortality rate by quartile (quartile 1 means richer area)
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = ratio, color = pov_quartile))

# MHI GRAPHS
#plot mortality rate by med hh income quartiles
# DECENT
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = ratio, color = medhh_quartile))


ggplot(data = maindf) +
  geom_point(mapping = aes(x = poverty_percent_all_ages, y = ratio, color = medhh_quartile))

# GOOD !!! I LIKE THIS
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = poverty_percent_all_ages, y = ratio, fill = medhh_quartile), 
           stat = "identity",
           position = "stack")
# ??? What is going on here
ggplot(data = maindf) +
  geom_tile(mapping = aes(x = pov_quartile, y = medhh_quartile, fill = ratio))


# UNEMPLOYMENT
# nothing learned from this ?
ggplot(data = maindf) + 
  geom_density(mapping = aes(x = ratio, color = unemp_quartile))


# NOTICE mean mortality increases with unemployment rate but not by much
fact_rat <- as.factor(ifelse(maindf$ratio <= 0.010215378, '1', 
                                          ifelse(maindf$ratio <= 0.013470173, '2', 
                                                 ifelse(maindf$ratio <= .017370823, '3', 
                                                        ifelse(maindf$ratio <= 0.5, '4')))))
# Ehhh it might be okay if we get rid of outliers?
ggplot(data = maindf, aes(x = unemp_percent, y = poverty_percent_all_ages,
                          color = fact_rat, alpha = .5)) +
  geom_point() +
  geom_smooth(method = "lm")

# I LIKE THIS
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = fact_rat, fill = pov_quartile),
           position = "stack",
           nbins = 5,
           binwidth = 1)
# I LIKE THIS TOO
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = fact_rat, fill = medhh_quartile),
           position = "stack",
           nbins = 5,
           binwidth = 1)
# AND THIS <3
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = fact_rat, fill = unemp_quartile),
           position = "stack",
           nbins = 5,
           binwidth = 1)

# RACE GRAPHS ?

# EDUCATION GRAPHS
# eh?
ggplot(data = maindf) +
  geom_point(mapping = aes(x = no_hs_diploma, y = ratio))

#completed high school
# this is nice
ggplot(data = maindf) +
  geom_point(mapping = aes(x = hs_diploma, y = ratio, color = pov_quartile), alpha = .5)

#Bachelor's Degree
# and this is nice too
ggplot(data = maindf) +
  geom_point(mapping = aes(x = bachelors, y = ratio, color = pov_quartile), alpha = .5)
# ehh
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = ratio_quartile, y = ..count.., fill = bachelors_quartile))
# YES !
ggplot(data = maindf) +
  geom_density(mapping = aes(x = ratio, color = bachelors_quartile))
ggplot(data = maindf) +
  geom_density(mapping = aes(x = bachelors, color = ratio_quartile))
# YESSS @ these two
ggplot(data = maindf) +
  geom_histogram(mapping = aes(x = ratio, fill = bachelors_quartile), 
                 position = "dodge", bins = 20, binwidth = .05)
ggplot(data = maindf) +
  geom_histogram(mapping = aes(x = bachelors, fill = ratio_quartile,))

temp <- filter(maindf,substr(FIPS,1,2)=="20")
ggplot(data = temp) +
  geom_bar(mapping = aes(x = reorder(factor(FIPS), ratio), y = ratio, fill = white_quartile), stat = "identity") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplot(data = temp) +
  geom_bar(mapping = aes(x = reorder(factor(FIPS), ratio), y = ratio, fill = black_quartile), stat = "identity") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = ratio_quartile, y = ..count.., fill = white_quartile))
ggplot(data = maindf) +
  geom_bar(mapping = aes(x = ratio_quartile, y = ..count.., fill = black_quartile))

ggplot(data = maindf, mapping = aes(x = poverty_percent_all_ages, y = ratio, color = bachelors_quartile)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 9, color = "gray") +
  geom_vline(xintercept = 12, color = "gray") +
  geom_vline(xintercept = 16, color = "gray")

ggplot(data = maindf, mapping = aes(x = poverty_percent_all_ages, y = ratio, color = bachelors_quartile)) +
  geom_point(alpha = .5) +
  facet_wrap(~ bachelors_quartile)

ggplot(data = maindf, mapping = aes(x = poverty_percent_all_ages, y = ratio, color = medhh_quartile)) +
  geom_point(alpha = .5) +
  facet_wrap(~ medhh_quartile)

ggplot(data = maindf, mapping = aes(x = median_household_income, y = ratio, color = white_quartile)) +
  geom_point(alpha = .5) +
  facet_wrap(~ white_quartile) +
  geom_hline(yintercept = mean(maindf$ratio[maindf$white_quartile]))

ggplot(data = maindf, mapping = aes(x = median_household_income, y = ratio, color = black_quartile)) +
  geom_point(alpha = .5) +
  facet_wrap(~ black_quartile) 

ggplot(data = maindf, mapping = aes(x = median_household_income, y = ratio, color = asian_quartile)) +
  geom_point(alpha = .5) +
  facet_wrap(~ asian_quartile) 

ggplot(data = maindf, mapping = aes(x = median_household_income, y = ratio, color = hispanic_quartile)) +
  geom_point(alpha = .5) +
  facet_wrap(~ hispanic_quartile) 

### TREND IN MORTALITY RATE GRAPHS ###

#attempt to find mortality rate for each day
# get rid of unnecessary columns from deaths and cases
deaths <- deaths[, -c(1:4, 6, 8:54)]
cases <- cases[, -c(1:4, 6, 8:53)]

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
mr_trend_bac <- left_join(pov, mr_trend, by = "FIPS")
mr_trend_bac <- na.omit(mr_trend_bac)
temp2 <- select(maindf, FIPS, bachelors_quartile)
mr_trend_bac <- left_join(temp2, mr_trend_bac, by = "FIPS")

#aggregate df to find avg mortality rate each day w/in the quartiles
mr_trend_agg <- aggregate(mr_trend_bac, by = list(mr_trend_bac$bachelors_quartile), FUN = mean)
is.na(mr_trend_agg) <- sapply(mr_trend_agg, is.infinite)
mr_trend_agg[is.na(mr_trend_agg)] <- 0
#want x axis to be day and y axis to be average and color by quartile
transpose <- t(mr_trend_agg)
transpose <- transpose[-c(1:5), ]
colnames(transpose) <- c("Quartile_1", "Quartile_2", "Quartile_3", "Quartile_4")

dates <- read.csv("/Users/abbyeast/Desktop/fall 22/stat3355/project/data/Dates.csv", 
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
  labs(x = "Date", y = "Average Mortality Rate", title = "Mortality Rate By Bachelors Quartile")

### MAPS ??????? ###
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
high_pov <- select(high_pov, FIPS, Lat, long)
high_pov <- filter(high_pov, str_starts(high_pov$FIPS, '15') != TRUE)
high_pov <- filter(high_pov, str_starts(high_pov$FIPS, '02') != TRUE)
high_pov$Lat <- unlist(high_pov$Lat)
high_pov$long <- unlist(high_pov$long)

high_ratio <- filter(maindf, as.integer(ratio_quartile) >= 3)
high_ratio <- select(high_ratio, FIPS, Lat, long, ratio_quartile)
high_ratio <- filter(high_ratio, str_starts(high_ratio$FIPS, '15') != TRUE)
high_ratio <- filter(high_ratio, str_starts(high_ratio$FIPS, '02') != TRUE)
high_ratio$Lat <- unlist(high_ratio$Lat)
high_ratio$long <- unlist(high_ratio$long)

low_pov <- filter(maindf, as.integer(pov_quartile) < 3)
low_pov <- select(low_pov, FIPS, Lat, long)
low_pov <- filter(low_pov, str_starts(low_pov$FIPS, '15') != TRUE)
low_pov <- filter(low_pov, str_starts(low_pov$FIPS, '02') != TRUE)
low_pov$Lat <- unlist(low_pov$Lat)
low_pov$long <- unlist(low_pov$long)

low_ratio <- filter(maindf, as.integer(ratio_quartile) < 3)
low_ratio <- select(low_ratio, FIPS, Lat, long, ratio_quartile)
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
high_white <- select(high_white, FIPS, Lat, long)
high_white <- filter(high_white, str_starts(high_white$FIPS, '15') != TRUE)
high_white <- filter(high_white, str_starts(high_white$FIPS, '02') != TRUE)
high_white$Lat <- unlist(high_white$Lat)
high_white$long <- unlist(high_white$long)

# BlACK
high_black <- filter(maindf, as.integer(black_quartile) >= 3)
high_black <- select(high_black, FIPS, Lat, long)
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
hs_cor <- cor(as.integer(maindf$hs_diploma), maindf$ratio, method = "pearson") # 0.3498861
nohs_cor <- cor(as.integer(maindf$no_hs_diploma), maindf$ratio, method = "pearson") # 0.3134558
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
maindf$no_hs_diploma <- as.numeric(maindf$no_hs_diploma)
maindf$hs_diploma <- as.numeric(maindf$hs_diploma)
groups <- select(maindf, ratio, poverty_percent_all_ages, median_household_income, unemp_percent,
                 White_perc, Black_perc, Asian_perc, Hispanic_perc, 
                 bachelors, no_hs_diploma, hs_diploma)
groups_corrs <- cor(groups, method = "spearman")
groups_corrp <- cor(groups, method = "pearson")
corrplot(abs(groups_corrp), method = "color", tl.col = "black", tl.cex = .75, is.corr = FALSE,
         cl.lim=c(0,.5), col=colorRampPalette(c("steelblue", "lightblue1"))(200))
corrplot(abs(groups_corrs), method = "color", tl.col = "black", tl.cex = .75, is.corr = FALSE,
         cl.lim=c(0,1), col=colorRampPalette(c("steelblue", "lightblue1"))(200))
