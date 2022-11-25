library(ggplot2)

states <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/dataset/coviddata.txt", 
                     header = TRUE, sep = ",", fill = TRUE)
countries <- read.table("/Users/abbyeast/Desktop/fall 22/stat3355/dataset/country_daily_june.txt", 
                        quote = "\"", header = TRUE, sep = ",", fill = TRUE)

emptyst <- names(states) %in% c("Recovered", "Active", "Total_Test_Results",
                              "People_Hospitalized", "Testing_Rate", "Hospitalization_Rate",
                              "Country_Region", "FIPS", "ISO3", "UID")
# note: keep only 50 states + PR, do this later?
states <- states[!emptyst]
colnames(states)[1] <- "Country_Region"
states$Recovered <- states$Confirmed - states$Deaths
rownames(states) = c(1:nrow(states))

emptyct <- names(countries) %in% c("Recovered", "Active", "Combined_Key")
countries <- countries[!emptyct]
countries <- countries[countries$Country_Region == "US", ]
countries$Province_State <- as.factor(countries$Province_State)

# Compare Texas, California, Florida, Ohio, New York
some_states <- subset(states, 
                      states$Country_Region == "Texas" | 
                        states$Country_Region == "California" |
                        states$Country_Region == "New York" |
                        states$Country_Region == "Ohio" |
                        states$Country_Region == "Florida")
rownames(some_states) = c(1:nrow(some_states))

# Case fatality ratio...
ggplot(data = some_states, aes(x = Country_Region, y = Case_Fatality_Ratio)) +
  geom_bar(stat = "identity", fill = c(2:6), alpha = 0.75)

# Confirmed cases per state
ggplot(data = some_states, aes(x = Country_Region, y = Confirmed)) +
  geom_bar(stat = "identity", fill = c(2:6), alpha = 0.75)

# Normalized confirmed cases per state
# ISSUE: Can't seem to get good state populations into a dataset lol?????
# Note: THIS IS FROM 2019 DATA
# Contains all 50 states
pops <- read.table("~/Desktop/fall 22/stat3355/dataset/state_pops.txt", 
                   header = FALSE, sep = ",")
names(pops) <- c("State", "Year", "Population")
pops <- subset(pops, Year == "2019")
some_pops <- subset(pops,
               pops$State == "TX" |
                 pops$State == "CA" |
                 pops$State == "NY" |
                 pops$State == "OH" |
                 pops$State == "FL" )
# Norm_Cases = ratio of confirmed / population for each state
some_states$Norm_Cases <- round(((some_states$Confirmed / some_pops$Population) * 100),
                                digits = 2)
some_states$Norm_Deaths <- round(((some_states$Deaths / some_pops$Population) * 100),
                                digits = 2)
some_states$Norm_Recovered <- round(((some_states$Recovered / some_pops$Population) * 100),
                                digits = 2)
ggplot(data = some_states, aes(x = Country_Region, y = Norm_Cases)) +
  geom_bar(stat = "identity", fill = c(2:6), alpha = 0.75)

# Number cases per 100 residents?

# Stacked bar plot: deaths and confirmed cases
xval <- c()
for(i in 1:nrow(some_states)) {
  new_val <- rep(some_states$Country_Region[i], 2)
  xval <- c(xval, new_val)
}
yval <- c()
for(j in 1:nrow(some_states)) {
    yval <- c(yval, (some_states[j,]$Confirmed - some_states[j,]$Deaths))
    yval <- c(yval, some_states[j,]$Deaths)
}
groups <- rep(c("Recoveries", "Fatalities"), nrow(some_states))
graphdf <- data.frame(xval, yval, groups)
options(scipen = 3)
ggplot(data = graphdf, aes(x = xval, y = yval, fill = groups)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "States", y = "Number of cases", title = "Total cases by state")

# Percent stacked bar plot
ggplot(data = graphdf, aes(x = xval, y = yval, fill = groups)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "States", y = "Number of cases", title = "Total cases by state")

# Stacked bar plot using norm
normyval <- c()
for(j in 1:nrow(some_states)) {
  normyval <- c(normyval, (some_states[j,]$Norm_Cases - some_states[j,]$Norm_Deaths))
  normyval <- c(normyval, some_states[j,]$Norm_Deaths)
}
groups <- rep(c("Recoveries", "Fatalities"), nrow(some_states))
normgraphdf <- data.frame(xval, yval, groups)
options(scipen = 3)
ggplot(data = normgraphdf, aes(x = xval, y = normyval, fill = groups)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "States", y = "Number of cases", title = "Total NORM cases by state")

# Percent stacked bar graph NORM
ggplot(data = normgraphdf, aes(x = xval, y = normyval, fill = groups)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "States", y = "Number of cases", title = "Total cases by state")


# Fatalities using norm
# something here
ggplot(data = some_states, aes(x = Country_Region, y = Norm_Deaths)) +
  geom_bar(stat = "identity", fill = c(2:6), alpha = 0.75)

