#install.packages("lubridate")
#install.packages("dplyr")
library(lubridate)
library(dplyr)

rm(list = ls())

#import data file
budburst_data <- read.csv("../data/phenology.csv")

#in the data, there are scores that have greater than symbols instead of just a number. This converts it into a number.
budburst_data$score <- gsub("<1", "0", budburst_data$score)
budburst_data$score <- gsub(">1", "1", budburst_data$score)
budburst_data$score <- gsub("<2", "1", budburst_data$score)
budburst_data$score <- gsub(">2", "2", budburst_data$score)
budburst_data$score <- gsub("<3", "2", budburst_data$score)
budburst_data$score <- gsub(">3", "3", budburst_data$score)
budburst_data$score <- gsub("<4", "3", budburst_data$score)
budburst_data$score <- gsub("<$", "3", budburst_data$score)
budburst_data$score <- gsub(">4", "4", budburst_data$score)
budburst_data$score <- gsub(">$", "4", budburst_data$score)
budburst_data$score <- gsub("<5", "4", budburst_data$score)
budburst_data$score <- gsub(">5", "5", budburst_data$score)
budburst_data$score <- gsub("<6", "5", budburst_data$score)
budburst_data$score <- gsub(">6", "6", budburst_data$score)
budburst_data$score <- gsub("<7", "7", budburst_data$score)
budburst_data$score <- gsub(">7", "7", budburst_data$score)
budburst_data$score <- gsub("<8", "8", budburst_data$score)
budburst_data$score <- gsub(">8", "8", budburst_data$score)
budburst_data$score <- gsub("<9", "9", budburst_data$score)
budburst_data$score <- gsub(">9", "9", budburst_data$score)
budburst_data$score <- gsub("<10", "10", budburst_data$score)
budburst_data$score <- gsub(">10", "10", budburst_data$score)

#format the score as a numeric value
budburst_data$score <- as.numeric(budburst_data$score)

#format the date as a proper date
budburst_data$date <- as.Date(budburst_data$date, format = "%d/%m/%Y")

#getting datapoints that have a score greater than 1 - greater than or equal to 2
agg_data <- budburst_data %>%
  group_by(date) %>%
  summarise(total_over_1 = sum(score > 1),
            total_trees = n())

#getting the day's percentage for trees that have a greater score than 1
percentage_over_1 <- agg_data$total_over_1 / agg_data$total_trees * 100

#getting a new dataframe to get the first day of the year that has more than 50% of trees with a score of 2.
filtered_data <- budburst_data %>%
  filter(date %in% agg_data$date[percentage_over_1 > 50]) %>%
  mutate(Year = year(date)) %>%
  group_by(Year) %>%
  filter(row_number() == 1)

filtered_data <- filtered_data %>% select(-note)

export_data <- filtered_data[, c("Year", "date")]

write.csv(export_data, "../results/budburst_day.csv", row.names = FALSE)
