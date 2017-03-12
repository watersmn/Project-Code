# Data Wrangling for Amazon Price versus In-Store Project
# Ming Waters

library(dplyr)
library(tidyr)
library(ggplot2)
library(outliers)

# Load original csv file and rename to w_price

w_price <- read.csv("original_amazon_compare.csv")

# Select needed columns 

w_price <- select(w_price, price, price_amazon, sale_online, datediff, PRICETYPE, category)

# Add new column to look for typos by looking for large difference in price and price_amazon)

w_price <- mutate(w_price, "delta" = (price - price_amazon))

# Add new column to calculate % price difference between store price 
# versus amazon price (Percentages will allow us to not neglect low cost
# items)

w_price <- mutate(w_price, "p_difference" = (delta/price))

# Graph price difference to visualize data entry errors
# determining what setting to set threshold for exclusion
ggplot(w_price, aes(x = category, y = p_difference, col = category)) + geom_point()

# Visualization showed that the outliers seem to be points where p_difference 
# < -100 
 
# Create vector with values < 100 p_difference naming vector error
error <- which(w_price$p_difference < -100)

# Remove values from dataset
w_price2 <- w_price[-error,]

# Plot new data set to see range of category and verify erronous have been removed
ggplot(w_price2, aes(x = category, y = p_difference, col = category)) + geom_point()

# Create data frame by category type
electronics_set <- filter(w_price2, category == "Electronics")

# Plot histogram to view distribution 
ggplot(electronics_set, aes(x = p_difference)) + geom_histogram(binwidth = 0.5)

# Run Grubbs test for two opposite outlier 
grubbs.test(electronics_set$p_difference, type = 11, opposite = FALSE, two.sided = TRUE)

# Output Grubbs test for two opposite outliers
#data:  electronics_set$p_difference
#G = 14.48300, U = 0.75709, p-value < 2.2e-16
#alternative hypothesis: -79.0790790790791 and 0.98919989199892 are outliers

# Create vector to remove outliers and test Grubbs test again
error2 <- which(electronics_set$p_difference > 0.98 | electronics_set$p_difference < -78)

# Remove outliers
electronics_set <- electronics_set[-error2,]


