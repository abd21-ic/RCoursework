#install.packages("lubridate")
#install.packages("dplyr")
library(lubridate)
library(dplyr)

rm(list = ls())

weatherdata <- read.csv("../data/SilwoodWeatherDaily.csv")

weatherdata$TIMESTAMP <- dmy_hm(weatherdata$TIMESTAMP)
weatherdata$TIMESTAMP <- as_date(weatherdata$TIMESTAMP)

# Extract year and month
weatherdata$Year <- year(weatherdata$TIMESTAMP)
weatherdata$Month <- month(weatherdata$TIMESTAMP)

weatherdata <- weatherdata %>%
  rename(Grass_Temp = Grass_Temp_Max..Deg.C...Max.)

filter(!is.na(Grass_Temp))

# Filter for April between 2010 and 2022
filtered_weather_data <- weatherdata %>%
  filter(Year >= 2009 & Year <= 2025, Month >= 3 & Month <= 5)

# Calculate average temperature for each year
averages <- filtered_weather_data %>%
  group_by(Year) %>%
  summarize(AverageGrassTemperature = mean(Grass_Temp))

averages
