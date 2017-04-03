# Data Wrangling for Amazon Price versus In-Store Project # Ming Waters

library(dplyr)
library(tidyr)
library(ggplot2)
library(outliers)

#Change working directory
setwd("C:/Users/IAZ867/Desktop/Desktop/Class/Assignments")
 
# Load original csv file and rename to w_price

 w_price <- read.csv("original_amazon_compare.csv")

# Select needed columns 
 
w_price <- select(w_price, id, price, price_amazon, sale_online, datediff, PRICETYPE, category)

# Add new column to calculate % price difference between store price 

w_price <- mutate(w_price, "delta" = (price_amazon - price))

# versus amazon price (Percentages will allow us to not neglect low cost items)
w_price <- mutate(w_price, "p_difference" = (delta/price))

# Create a vector to determine if id transfered over to price
id_typo <- which(w_price$id == w_price$price)

# two rows contain match 1034, 1614, remove the rows
w_price <- w_price[-id_typo, ]

# Graph price difference to visualize data entry errors
# determining what setting to set threshold for exclusion
ggplot(w_price, aes(x = category, y = p_difference, col = category)) + geom_point()

# Visualization showed that the outliers seem to be points where p_difference 
# > 100 
 
# Create vector with values < 100 p_difference naming vector error
error <- which(w_price$p_difference > 100)

# Remove values from dataset
w_price2 <- w_price[-error,]

# Plot new data set to see range of category and verify erronous have been removed
ggplot(w_price2, aes(x = category, y = p_difference, col = category)) + geom_point()

# Create data frames by category type
electronics_set <- filter(w_price2, category == "Electronics")
home_app <- filter(w_price2, category == "Home and Appliances")
mix <- filter(w_price2, category == "Mix")
office <- filter(w_price2, category =="Office Products")
pharm_health <- filter(w_price2, category == "Pharmacy and Health")

# Plot histogram to view distribution across categories and to determine spread
ggplot(electronics_set, aes(x = p_difference)) + geom_histogram(binwidth = 1.0)

# Electronic sets has a skewed negative bias with a few outliers that need to be 
# investigated on the high positive side
median(electronics_set$p_difference, na.rm = TRUE)

#median = -0.04 data is shows amazon price bias
ggplot(home_app, aes(x = p_difference)) + geom_histogram(binwidth = 1.0)

#home appliances has extreme values on positive side and a median with zero
median(home_app$p_difference, na.rm = TRUE)


ggplot(mix, aes(x = p_difference)) + geom_histogram(binwidth = 0.5)
# data shows positive skewed with extreme values
median(mix$p_difference, na.rm = TRUE)
# mix data shows in store as price value 0.165

ggplot(office, aes(x = p_difference)) + geom_histogram(binwidth = 0.5)
# office shows small counts of positive extreme values
median(office$p_difference, na.rm = TRUE)
# office shows amazon price value -0.111

ggplot(pharm_health, aes(x = p_difference)) + geom_histogram(binwidth = 0.5)
# data shows multiple counts of postitive extremes with large postive skew
median(pharm_health$p_difference, na.rm = TRUE)
# median shows price value for in store 2.030

# Data is not normal and traditional outlier test such as Grubbs, chi^2 will 
# not apply, a random inspection of data set shows multiple obvious typos
# View extreme positives of electronics
View(electronics_set)
# Row 153 shows -0.90 percent difference, inspection of values shows an
# in store price of 69.99 versus an amazon price of 6.98, most items would not
# have such a significance difference, the it would be resonable to conclude this
# was a typo since the values are so similar but with a difference in decimal placement

# To view outliers for non-normal data, the median IQR 1.5 removal approach will be taken

# visualize boxplot
ggplot(w_price2, aes(factor(category), p_difference)) + geom_boxplot()
# UglyPlots

# Apply IQR to electronic data set
eI <- 1.5*(IQR(electronics_set$p_difference))

# Get quartiles
summary(electronics_set$p_difference)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.98920 -0.23610 -0.04924  0.40260  0.00000 79.08000 

# Calculate upper and lower thresholds
e_upper <- (0+eI)
e_lower <- (-0.23 - eI)
# lower = -0.584, upper = 0.354 
# inspect data to determine if they are valid outliers
# setting lower boundaries at < - 0.590  due to the price at -0.58 seem like a 
# reasonable price difference due to the sale notation 
# on the upper side removing outliers > 0.354 seems reasonable as the price gaps between 
# items seem questionable and no items were on sale and price descriptions in original file
# indicates this is a good threshold

# Create vector to remove outliers
>out_ele <- which(electronics_set$p_difference < -0.590 | electronics_set$p_difference > 0.354)
 
# Remove from set
electronics_set <- electronics_set[-out_ele,]
# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(electronics_set$p_difference)


# Repeat for home_app
# Apply IQR to electronic data set
hI <- 1.5*(IQR(home_app$p_difference))

# IQR*1.5 = 0.3377

# Get quartiles
summary(home_app$p_difference)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9999 -0.1000  0.0000  1.4930  0.1252 89.7900

# Calculate upper and lower thresholds
h_upper <- (0.1252 + hI)
h_lower <- (-0.1 - hI)

# lower = -0.4377, upper = 0.46293 
# inspect data and product type to determine if they are valid outliers

# Create vector to remove outliers
out_app <- which(home_app$p_difference < -0.-4378 | home_app$p_difference > 0.462)

# Remove from set
home_app <- home_app[-out_app,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(home_app$p_difference)

# Reoeat for mix data
# Apply IQR to mix data set
mI <- 1.5*(IQR(mix$p_difference))

# Get quartiles
summary(mix$p_difference)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.97580 -0.09099  0.16520  1.57000  1.50200 51.51000 

# Calculate upper and lower thresholds
m_upper <- (1.50200 + mI)
m_lower <- (-0.09099 - mI)

# lower = -2.4801, upper = 3.891176
# inspect data to determine if they are valid outliers
# Data riddled with typos....for example cost of chapstic in store 1.98 versus 253.00 on amazon, 
# inspection showed suspect errors for differenes > 2


# Create vector to remove outliers
out_mix <- which(mix$p_difference < -2.48 | mix$p_difference > 2.0)

# Remove from set
mix <- mix[-out_mix,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(mix$p_difference)

# Repeat for office data
# Apply IQR to office data set
oI <- 1.5*(IQR(office$p_difference))

# Get quartiles
summary(office$p_difference)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9600 -0.3433 -0.1112  0.5264  0.5104 14.2400 

# Calculate upper and lower thresholds
o_upper <- (0.5104 + oI)
o_lower <- (-0.3433 - oI)

# lower = -1.6239, upper = 1.791
# inspect data to determine if they are valid outliers
# Data showed multiple entries of same item and small items like amazon cables


# Create vector to remove outliers
out_office <- which(office$p_difference < -1.6239 | office$p_difference > 1.791)

# Remove from set
office <- office[-out_office,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(office$p_difference)


# Repeat for Pharm_health data
# Apply IQR to office data set
pI <- 1.5*(IQR(pharm_health$p_difference))

# Get quartiles
summary(pharm_health$p_difference)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.8004 -0.1328  2.0250  8.0040  8.8560 80.7400 

# Calculate upper and lower thresholds
p_upper <- (8.8560 + pI)
p_lower <- (-0.1328 - pI)

# lower = -13.61622, upper = 22.3394
# inspect data to determine if they are valid outliers
# Data showed multiple lots of odd high pricing for amazon items that show very low prices in store
# ***Positive delta doesn't make sens until p_difference is < 2.41


# Create vector to remove outliers
out_pharm <- which(pharm_health$p_difference < -13.62 | pharm_health$p_difference > 22.34)

# Remove from set
pharm_health <- pharm_health[-out_pharm,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(pharm_health$p_difference)

# Recombind the various categories into one data frame
w_price3 <- rbind(electronics_set, home_app, pharm_health, mix, office)

