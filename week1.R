library(tidyverse)

# Import USDA dataset

library(readr)
USDA <- read_csv("~/Documents/Online Classes/analytics-edge/USDA.csv")


str(USDA)
summary(USDA)
glimpse(USDA)


# Get the max sodium
max(USDA$Sodium, na.rm = TRUE)

# Which food has the highest/max sodium content? HINT: use the which.max() funxn
which.max(USDA$Sodium) # gives the index number 265
USDA$Description[265] # to get the name of the food

# Create a new df of high sodium foods (> 10,000 mg)

HighSodium <- USDA %>% filter(Sodium > 10000)

# Alternative method highlighted in the lecture
# HighSodium <- subset(USDA, Sodium > 10000)

# How many foods have Sodium > 10,000 mg per serving?
nrow(HighSodium)

# Which foods have Sodium > 10,000 mg?
HighSodium$Description

# How much Sodium is there in caviar?
USDA %>% filter(grepl("CAVIAR", Description)) %>% select(Sodium)

# Alternative method shown in lecture
match("CAVIAR", USDA$Description) # gives caviar as index 4154
USDA$Sodium[4154]

# What is the mean and std dev Sodium level for all of the foods in our USDA dataset?
mean(USDA$Sodium, na.rm = TRUE)
sd(USDA$Sodium, na.rm = TRUE)

# Create a scatterplot of Protein (x) and Total Fat (y)
plot(USDA$Protein, USDA$TotalFat)

USDA %>% ggplot(aes(x = Protein, y = TotalFat)) + geom_point()

# Create a histogram for Vitamin C
hist(USDA$VitaminC, 
     xlab = "Vitamin C (mg)", 
     main = "Histogram of Vitamin C Levels", 
     xlim = c(0,100),
     breaks = 2000
     )

# Create a boxplot for Sugar

boxplot(USDA$Sugar,
        main = "Boxplot of Sugar levels"
        )

# Create a new variable that = 1 if high sodium than avg or = 0 if less than average

HighSodium <- USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE)
str(HighSodium) # shows that this is a Logical output

# Convert to numeric w/ as.numeric
as.numeric(HighSodium)

# Add HighSodium to USDA
USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))

#Alternative approach would be to use cbind()
#USDA <- cbind(USDA, HighSodium)

# Now, the tidy approach

USDA <- USDA %>% 
  mutate(HighSodium = as.numeric(Sodium > mean(Sodium, na.rm = TRUE)))

# Using a similar approach, make HighProtein, HighFat, High

USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbohydrate <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))


# Use table to see how many foods are high vs low sodium

table(USDA$HighSodium)

# Now make a table comparing foods w/ high vs low sodium AND fat

table(USDA$HighSodium, USDA$HighFat)

# Calculate the average amount of iron sorted by high and low protein using tapply()

# tapply(arg1, arg2, arg3) - groups arg1 by arg2, then applies arg3

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)

USDA %>% group_by(HighProtein) %>% summarise(mean = mean(Iron, na.rm = TRUE))

# Calculate the max amount of Vitamin C in foods w/ high and low carbs

tapply(USDA$VitaminC, USDA$HighCarbohydrate, max, na.rm = TRUE)

USDA %>% group_by(HighCarbohydrate) %>% summarise(max = max(VitaminC, na.rm = TRUE))

# Calculate summary stats of Vitamin C in foods w/ low and high carbs

tapply(USDA$VitaminC, USDA$HighCarbohydrate, summary, na.rm = TRUE)
