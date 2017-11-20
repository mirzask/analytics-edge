library(tidyverse)
library(readr)

climate <- read_csv("~/Documents/Online Classes/analytics-edge/climate_change.csv")

# Split the data

climate_train <- subset(climate, climate$Year <= 2006)

climate_test <- subset(climate, climate$Year > 2006)

# Build a linear regression model to predict the dependent variable Temp, 
# using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variables

model_train <- lm(Temp ~ MEI + CO2 + CH4 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, data = climate_train)

summary(model_train)

# Compute the correlations between all the variables in the training set.

cor(climate_train)

library(corrplot)
corrplot(cor(climate_train), method = "shade")

# Focus on the N2O variable and build a model 
# with only MEI, TSI, Aerosols and N2O as independent variables

model_train2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climate_train)
summary(model_train2)

# step function
model_step <- step(model_train)
summary(model_step)

# Using the model produced from the step function, calculate temperature predictions for the testing data set

predict_climate <- predict(model_step, newdata=climate_test)
predict_climate

# Calculate R^2

RSS <- sum((climate_test$Temp - predict_climate)^2)
TSS <- sum((climate_test$Temp - mean(climate_train$Temp))^2)
1 - RSS/TSS



# Reading Test Scores

# Load the datasets
library(readr)
library(tidyverse)
pisa <- read_csv("~/Documents/Online Classes/analytics-edge/pisa2009train.csv")
pisa_test <- read_csv("~/Documents/Online Classes/analytics-edge/pisa2009test.csv")

nrow(pisa)

# What is the average reading test score of males?

glimpse(pisa)

tapply(pisa$readingScore, pisa$male, mean)

# Which variables are missing data in at least one observation in the training set?

summary(pisa)


pisa <- na.omit(pisa)

pisa_test <- na.omit(pisa_test)

# How many observations are now in the training set? test set?

nrow(pisa)
nrow(pisa_test)

glimpse(pisa)

# Set race as a factor and set "White" as the reference level

pisa$raceeth <- as.factor(pisa$raceeth)
pisa_test$raceeth <- as.factor(pisa_test$raceeth)

pisa$raceeth = relevel(pisa$raceeth, "White")
pisa_test$raceeth = relevel(pisa_test$raceeth, "White")

# Build a linear regression model (call it lmScore) using the training set 
# to predict readingScore using all the remaining variables

lmScore <- lm(readingScore ~., data=pisa)
summary(lmScore)

RSS <- sum(lmScore$residuals^2)
RMSE <- sqrt(RSS/nrow(pisa))
RMSE

# use the lmScore model to predict the reading scores of students in test set

predTest <- predict(lmScore, newdata = pisa_test)

# What is range between the maximum and minimum predicted reading score on the test set?

summary(predTest)
max(predTest) - min(predTest)

# What  is the sum of squared errors (RSS) of lmScore on the testing set?
# RMSE?

RSS <- sum((pisa_test$readingScore - predTest)^2)
RMSE <- sqrt(RSS/nrow(pisa_test))
RSS
RMSE


# What is the predicted test score used in the baseline model?

mean(pisa$readingScore)

# What is the RSS of the baseline model on the testing set?

TSS_base <- sum((pisa_test$readingScore-mean(pisa$readingScore))^2)
TSS_base


# What is the test-set R-squared value of lmScore?

RSS <- sum((pisa_test$readingScore - predTest)^2)
TSS_base <- sum((pisa_test$readingScore-mean(pisa$readingScore))^2)
1 - RSS/TSS_base


# Detecting Flu Epidemics via Search Engine Query Data

FluTrain <- read_csv("~/Documents/Online Classes/analytics-edge/FluTrain.csv")

# Looking at the time period 2004-2011, 
# which week corresponds to the highest percentage of ILI-related physician visits?

glimpse(FluTrain)
which.max(FluTrain$ILI)
FluTrain$Week[303]

# Plot the histogram of the dependent variable, ILI. 
# What best describes the distribution of values of ILI?

hist(FluTrain$ILI)

# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?
par(mfrow=c(1,2))
plot(FluTrain$ILI, FluTrain$Queries)
plot(log(FluTrain$ILI), FluTrain$Queries)

# Create a regression model to predict log(ILI) based on Queries.
# What is the training set R-squared value?

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

cor(log(FluTrain$ILI), FluTrain$Queries)^2


FluTest <- read_csv("~/Documents/Online Classes/analytics-edge/FluTest.csv")

#What is our estimate for the percentage of 
#ILI-related physician visits for the week of March 11, 2012?

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

FluTest$Week[11]

PredTest1[11]

# What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012?

(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

# RMSE b/w our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?

RSS <- sum((PredTest1 - FluTest$ILI)^2)
RMSE <- sqrt(RSS/nrow(FluTest))
RSS
RMSE


install.packages("zoo")
library(zoo)

ILILag2 = stats::lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

sum(is.na(ILILag2))

# plot the log of ILILag2 against the log of ILI.

par(mfrow=c(1,1))
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))


#Train a linear regression model on the FluTrain dataset to predict 
#the log of the ILI variable using the Queries variable as well as the
# log of the ILILag2 variable.

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)


# add an ILILag2 variable to the FluTest data frame. How many missing values are there in this new variable?

ILILag2 = stats::lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 = coredata(ILILag2)

sum(is.na(ILILag2))

# Obtain test set predictions of the ILI variable from the FluTrend2 model.
# What is the test-set RMSE of the FluTrend2 model?

predictTest2 <- exp(predict(FluTrend2, newdata = FluTest))

RSS <- sum((predictTest2 - FluTest$ILI)^2)
RMSE <- sqrt(RSS/nrow(FluTest))
RMSE
