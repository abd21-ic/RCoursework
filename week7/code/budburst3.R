#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("ggplot2")
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(dplyr)
library(lme4)

rm(list = ls())

#import data file
budburst_data_a <- read.csv("../data/phenology.csv")
tree <- read.csv("../data/trees.csv")

quercus_trees <- tree %>%
  filter(species == "quercus.robur") %>%
  select(TreeID)

budburst_data <- budburst_data_a %>%
  filter(TreeID %in% quercus_trees$TreeID)

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

export_bud_data <- budburst_data[, c("date", "score")]

write.csv(export_bud_data, "../results/budburst_day_by_day.csv", row.names = FALSE)

#format the score as a numeric value
budburst_data$score <- as.numeric(budburst_data$score)
budburst_data$date <- as.Date(budburst_data$date, format = "%d/%m/%Y")

budburst_data <- budburst_data %>%
  filter(year(date) > 2009)

# Identify the first day each tree reaches score >= 2
tree_budburst <- budburst_data %>%
  filter(score >= 2) %>%
  mutate(Year = year(date)) %>%
  group_by(TreeID, Year) %>%
  arrange(date, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, date, Year)

# Export for reference
write.csv(tree_budburst, "../results/budburst_day_per_tree.csv", row.names = FALSE)

# Now combine with weather data
weatherdata <- read.csv("../data/SilwoodWeatherDaily.csv")

weatherdata$TIMESTAMP <- dmy_hm(weatherdata$TIMESTAMP)
weatherdata$Year <- year(weatherdata$TIMESTAMP)
weatherdata$Month <- month(weatherdata$TIMESTAMP)
weatherdata$TIMESTAMP <- as.Date(weatherdata$TIMESTAMP)

weatherdata <- weatherdata %>%
  rename(
    Air_Temp = Air_Temp..Deg.C...Smp.,
    Grass_Temp_Smp = Grass_Temp..Deg.C...Smp.,
    Soil_Temp_2in = Soil_Temp_2in...Deg.C...Smp.,
    Soil_Temp_4in = Soil_Temp_4in...Deg.C...Smp.,
    Air_Temp_Max = Air_Temp_Max...Deg.C...Max.,
    Air_Temp_Min = Air_Temp_Min...Deg.C...Min.,
    Grass_Temp = Grass_Temp_Max..Deg.C...Max.,
    Grass_Temp_Min = Grass_Temp_Min..Deg.C...Min.,
    Rain_mm_Tot = Rain_mm_Tot...mm...Tot.
  )

tree_budburst <- budburst_data %>%
  filter(score >= 2) %>%
  mutate(Year = year(date)) %>%
  group_by(TreeID, Year) %>%
  arrange(date, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, date, Year)

weatherdata <- weatherdata %>%
  select(TIMESTAMP, Rain_mm_Tot) %>%
  arrange(TIMESTAMP)

get_30day_rain <- function(bud_date, weather_df) {
  start_date <- bud_date - 30
  weather_subset <- weather_df %>%
    filter(TIMESTAMP >= start_date & TIMESTAMP < bud_date)
  sum(weather_subset$Rain_mm_Tot, na.rm = TRUE)
}

tree_budburst <- tree_budburst %>%
  rowwise() %>%
  mutate(Rain_30d = get_30day_rain(date, weatherdata)) %>%
  ungroup() %>%
  mutate(Julian_Days = yday(date))

tree_lmer_rain_julian <- lmer(Julian_Days ~ Rain_30d + (1 | TreeID) + (1 | Year), data = tree_budburst)
summary(tree_lmer_rain_julian)

ggplot(tree_budburst, aes(x = Rain_30d, y = Julian_Days, color = factor(Year))) +
  geom_point(size = 1, position = position_jitter(width = 0.5, height = 0.5)) +  # points colored by year
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # trend line with standard error
  labs(
    x = "Total Rainfall 30 Days Before Budburst (mm)",
    y = "Julian Day of Budburst (per tree)",
    color = "Year"
  ) +
  theme_classic()

par(mfrow = c(2, 2))


