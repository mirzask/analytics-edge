library(tidyverse)
library(lubridate)

# CHICAGO MOTOR VEHICLE THEFTS

# Import Dataset
library(readr)
mvt <- read_csv("~/Documents/Online Classes/analytics-edge/mvtWeek1.csv")


glimpse(mvt)
nrow(mvt)
ncol(mvt)

max(mvt$ID)
min(mvt$Beat)
sum(mvt$Arrest == TRUE)
# Alternatively, could easily get using table(mvt$Arrest)

sum(mvt$LocationDescription == "ALLEY")


# Working with dates

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

# In which month did the fewest motor vehicle thefts occur?

table(mvt$Month)
which.min(table(mvt$Month))

# On which weekday did the most motor vehicle thefts occur?

table(mvt$Weekday)
which.max(table(mvt$Weekday))

# Which month has the largest number of motor vehicle thefts for which an arrest was made?

table(mvt$Month, mvt$Arrest)

# Histogram of motor vehicle thefts by Date

hist(mvt$Date, breaks=100)


# Boxplot of Arrests over time

boxplot(mvt$Date ~ mvt$Arrest)


# For what proportion of motor vehicle thefts in 2001 was an arrest made? 

table(mvt$Year, mvt$Arrest)
2152/(18517 + 2152)

# For what proportion of motor vehicle thefts in 2007 was an arrest made? 

table(mvt$Year, mvt$Arrest)
1212/(1212 + 13068)

# For what proportion of motor vehicle thefts in 2012 was an arrest made? 

table(mvt$Year, mvt$Arrest)
550/(550 + 13542)

# Which locations are the top five locations for motor vehicle thefts (excluding "Other")?

mvt %>% select(LocationDescription) %>% count(LocationDescription) %>% arrange(desc(n))

# Alternative approach:
# sort(table(mvt$LocationDescription))

# How many observations are in Top5?

TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")

mvt %>% filter(LocationDescription %in% TopLocations)

# NOTE: if you use mvt %>% filter(LocationDescription == TopLocations) you will get the wrong
# answer. If you want to use `==`, then you have to type it ALL out w/ |, |, |,...

mvt %>% filter(LocationDescription=="STREET" | 
                 LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
                 LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | 
                 LocationDescription=="DRIVEWAY - RESIDENTIAL")
# Base R approach would be Top5 = subset(mvt, LocationDescription %in% TopLocations)


# One of the locations has a much higher arrest rate than the other locations. Which is it?

table(Top5$Arrest, Top5$LocationDescription) # Get answer by calc which has the highest T/F ratio?

# On which day of the week do the most motor vehicle thefts at gas stations happen?

Top5 %>% filter(LocationDescription == "GAS STATION") %>% group_by(Weekday) %>% count()

# Alternative approach
# table(Top5$LocationDescription, Top5$Weekday)

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?

table(Top5$LocationDescription, Top5$Weekday)




# STOCK DYNAMICS

# Read the csv files - IBM, GE, PG, Coke, Boeing

IBM <- read_csv("~/Documents/Online Classes/analytics-edge/IBMStock.csv")
GE <- read_csv("~/Documents/Online Classes/analytics-edge/GEStock.csv")
ProcterGamble <- read_csv("~/Documents/Online Classes/analytics-edge/ProcterGambleStock.csv")
CocaCola <- read_csv("~/Documents/Online Classes/analytics-edge/CocaColaStock.csv")
Boeing <- read_csv("~/Documents/Online Classes/analytics-edge/BoeingStock.csv")

# Convert the Date data to something R can understand:

# as.Date(arg1, arg2)
# arg1 is the date we wish to convert
# arg2 is the format of the date variable in arg1

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")


dim(IBM)

# What is the earliest year in our datasets?

min(year(IBM$Date))


# What is the latest year in our datasets?

max(year(IBM$Date))

# What is the mean stock price of IBM over this time period?

mean(IBM$StockPrice)

# What is the minimum stock price of General Electric (GE) over this time period?

min(GE$StockPrice)


# What is the maximum stock price of Coca-Cola over this time period?

max(CocaCola$StockPrice)


# What is the median stock price of Boeing over this time period?

median(Boeing$StockPrice)


# What is the standard deviation of the stock price of Procter & Gamble over this time period?

sd(ProcterGamble$StockPrice)


# Plot - Around what year did Coca-Cola has its highest stock price in this time period? 
# lowest?

plot(CocaCola$Date, CocaCola$StockPrice, type="l") # type="l" gives  line graph

CocaCola %>% ggplot(aes(Date, StockPrice)) + geom_line()


# Plot - Add PG line to the CocaCola price chart from above

# Differentiate w/ diff colored lines
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

# Alternative diff would be diff line types:
plot(CocaCola$Date, CocaCola$StockPrice, type="l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, lty=2) # lty=2 gives dashed line
abline(v=as.Date(c("2000-03-01")), lwd=1, col="red")


# Plot all 5 companies stock prices and see how the prices changed from 1995-2005
colors() # gives all the possible colors

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], lty=2)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green", lty=3)
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="orange", lty=4)
abline(v=as.Date(c("2000-03-01")), lwd=1, col="red")
abline(v=as.Date(c("1997-09-01")), lwd=1, col="purple")
abline(v=as.Date(c("1997-11-01")), lwd=1, col="purple")


# Use the tapply command to calculate the mean stock price of IBM, sorted by months

tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

# GE and Coke both have their highest average stock price in the same month. Which month is it?

tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)





# Demographics and Employment in the United States

CPS <- read_csv("~/Documents/Online Classes/analytics-edge/CPSData.csv")

glimpse(CPS)

#what is the most common industry of employment?
sort(table(CPS$Industry))

# Which state has the fewest interviewees? Which has the most?

sort(table(CPS$State))


# What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship)
(116639 + 7073)/nrow(CPS)

# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?

table(CPS$Hispanic, CPS$Race)

# Which variables have at least one interviewee with a missing (NA) value?

apply(CPS, 2, anyNA)


# Any interviewee factors associated w/ NA for married?
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))


# How many states had all interviewees living in a non-metropolitan area? (ie NA for MetroCode)

table(CPS$State, is.na(CPS$MetroAreaCode))


# Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?

table(CPS$Region, is.na(CPS$MetroAreaCode))


# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean, na.rm = TRUE))


# How many observations (codes for metropolitan areas) are there in MetroAreaMap?

MetroAreaMap <- read_csv("~/Documents/Online Classes/analytics-edge/MetroAreaCodes.csv")

dim(MetroAreaMap)

# How many observations (codes for countries) are there in CountryMap?

CountryMap <- read_csv("~/Documents/Online Classes/analytics-edge/CountryCodes.csv")

dim(CountryMap)

# Combine CPS and MetroAreaMap

CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# all.x=TRUE means we want to keep all rows from the "x" data frame (CPS),
# even if some of the rows' MetroAreaCode doesn't match any codes in MetroAreaMap

glimpse(CPS)
table(is.na(CPS$MetroArea))


# Which of the following metropolitan areas has the largest number of interviewees?

sort(table(CPS$MetroArea))


# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# Number of metropo areas in the US with at least 20% of interviewees are Asian?

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))


# Which metro area has the SMALLEST proportion of interviewees without a high school diploma?

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))



# Combine CPS and CountryMap

CPS <- merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)
  

glimpse(CPS)

table(is.na(CPS$Country))

# Most common country of birth (excluding N America)?

sort(table(CPS$Country))

# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States?

sum(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", na.rm = TRUE) #gives 5409
sum(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" & CPS$Country != "United States", na.rm = TRUE)

1668/5409

# Which metro area has the highest number of ppl who were born in India?
# Brazil?
# Somalia?

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm = TRUE))

