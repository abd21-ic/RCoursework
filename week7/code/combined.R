#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")
install.packages("lme4")

library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)

# Clear environment
rm(list = ls())

# -------------------------------
# Load and clean budburst data
# -------------------------------

budburst_data_a <- read.csv("../data/phenology.csv")
tree <- read.csv("../data/trees.csv")

# Filter for Quercus robur trees
quercus_trees <- tree %>%
  filter(species == "quercus.robur") %>%
  select(TreeID)

budburst_data <- budburst_data_a %>%
  filter(TreeID %in% quercus_trees$TreeID)

# Convert scores with "<" or ">" to numeric
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

# Export day-by-day scores
export_bud_data <- budburst_data[, c("date", "score")]
write.csv(export_bud_data, "../results/budburst_day_by_day.csv", row.names = FALSE)

# Format columns
budburst_data$score <- as.numeric(budburst_data$score)
budburst_data$date <- as.Date(budburst_data$date, format = "%d/%m/%Y")
budburst_data <- budburst_data %>%
  mutate(Year = year(date))

# -------------------------------
# Get first stage 2 date per TreeID per year
# -------------------------------

stage2_data <- budburst_data %>%
  filter(score >= 2)

export_data <- stage2_data %>%
  group_by(TreeID, Year) %>%
  summarise(first_stage2_date = min(date), .groups = "drop") %>%
  mutate(
    Julian_Day = yday(first_stage2_date)   # Julian day of year
  )

write.csv(export_data, "../results/first_stage2_per_tree.csv", row.names = FALSE)

# -------------------------------
# Load and clean weather data
# -------------------------------

weatherdata <- read.csv("../data/SilwoodWeatherDaily.csv")

weatherdata$TIMESTAMP <- dmy_hm(weatherdata$TIMESTAMP)
weatherdata$TIMESTAMP <- as_date(weatherdata$TIMESTAMP)
weatherdata$Year <- year(weatherdata$TIMESTAMP)
weatherdata$Month <- month(weatherdata$TIMESTAMP)

# Rename relevant columns
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

# Filter for April and years 2010–2025
filtered_weather_data <- weatherdata %>%
  filter(Year >= 2010 & Year <= 2025, Month == 4)

# Calculate average temperature and rainfall per year
averages <- filtered_weather_data %>%
  group_by(Year) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric) & !Year, ~ round(.x, 1)))

# -------------------------------
# Combine per-tree budburst with weather
# -------------------------------

combined_data <- export_data %>%
  left_join(averages, by = "Year") %>%
  select(TreeID, Year, first_stage2_date, Julian_Day, Air_Temp, Air_Temp_Max, Air_Temp_Min, Grass_Temp, Rain_mm_Tot)

write.csv(combined_data, "../results/combined_data_per_tree_julian.csv", row.names = FALSE)

# -------------------------------
# Linear mixed model: Julian Day vs Air Temperature
# -------------------------------

lmm_model <- lmer(Julian_Day ~ Air_Temp + (1 | TreeID) + (1 | Year), data = combined_data)
summary(lmm_model)

# -------------------------------
# Plot Julian Day vs Air Temperature
# -------------------------------

ggplot(combined_data, aes(x = Air_Temp, y = Julian_Day)) +
  geom_jitter(color = "black", size = 2, width = 0.03, height = 0.03) +  # add jitter
  geom_smooth(method = "lm", se = TRUE, color = "black", level = 0.99) +  # 99% CI
  labs(
    x = "Average April Air Temperature (°C)",
    y = "Julian Budburst Day"
  ) +
  theme_classic()

# -------------------------------
# Diagnostic plots
# -------------------------------

residuals <- resid(lmm_model)
fitted <- fitted(lmm_model)

# Residuals vs Fitted
plot(fitted, residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, lty = 2, col = "red")

# Q-Q plot
qqnorm(residuals, main = "Normal Q-Q Plot")
qqline(residuals, col = "red")

# Scale-Location plot
sqrt_abs_resid <- sqrt(abs(residuals))
plot(fitted, sqrt_abs_resid,
     xlab = "Fitted values",
     ylab = "√|Residuals|",
     main = "Scale-Location Plot")
abline(h = mean(sqrt_abs_resid), lty = 2, col = "red")

# Histogram of residuals
hist(residuals, breaks = 30,
     main = "Histogram of Residuals",
     xlab = "Residuals")

mean_julian <- mean(combined_data$Julian_Day, na.rm = TRUE)
sd_julian <- sd(combined_data$Julian_Day, na.rm = TRUE)

min_julian <- min(combined_data$Julian_Day, na.rm = TRUE)
max_julian <- max(combined_data$Julian_Day, na.rm = TRUE)

min_temp <- min(combined_data$Air_Temp, na.rm = TRUE)
max_temp <- max(combined_data$Air_Temp, na.rm = TRUE)

min_temp
max_temp

combined_data %>%
  group_by(Year) %>%
  summarise(mean_temp = mean(Air_Temp, na.rm = TRUE)) %>%
  slice_max(mean_temp, n = 1)
