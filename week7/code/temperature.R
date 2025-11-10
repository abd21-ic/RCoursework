#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("ggplot2")
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

rm(list = ls())

weatherdata <- read.csv("../data/SilwoodWeatherDaily.csv")

head(weatherdata)

weatherdata$TIMESTAMP <- dmy_hm(weatherdata$TIMESTAMP)
weatherdata$TIMESTAMP <- as_date(weatherdata$TIMESTAMP)

# Extract year and month
weatherdata$Year <- year(weatherdata$TIMESTAMP)
weatherdata$Month <- month(weatherdata$TIMESTAMP)

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

# Filter for April between 2010 and 2025
filtered_weather_data <- weatherdata %>%
  filter(Year >= 2009 & Year <= 2025, Month == 4)

# Calculate average temperature for each year
averages <- filtered_weather_data %>%
  group_by(Year) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

# Round to 1dp
averages <- averages %>%
  mutate(across(where(is.numeric) & !Year, ~ round(.x, 1)))

# Combine with budburst.R
combined_data <- averages %>%
  left_join(export_data, by = "Year")

# removing unncessary columns and missing years
combined_data <- combined_data %>%
  filter(!is.na(date)) %>%
  select(-Record)

combined_data <- combined_data %>%
  mutate(
    April_Days = as.integer(date - make_date(Year, 4, 1))
  )

write.csv(combined_data, "../results/combined_data.csv")

# budburst day is likely to be correlated with air temperature, so we select for that in April.
# liner model to see the relationship between air temperature and budburst day

air_lm_model <- glm(April_Days ~ Air_Temp, data = combined_data)

summary(air_lm_model)

ggplot(combined_data, aes(x = Air_Temp, y = April_Days)) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", color = "black") +
  labs(
    x = "Average April Air Temperature (°C)",
    y = "Days Since April 1",
    title = ""
  ) +
  theme_classic()

dev.off()

par(mfrow=c(2,2))
plot(air_lm_model)

