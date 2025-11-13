#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("lme4")
library(lubridate)
library(dplyr)
library(zoo)
library(lme4)

rm(list = ls())

#import data file
budburst <- read.csv("../data/phenology.csv")
tree <- read.csv("../data/trees.csv")
SilwoodWeatherDaily <- read.csv("../data/SilwoodWeatherDaily.csv")

budburst_data <- merge(budburst, tree, by = "TreeID")


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

SilwoodWeatherDaily <- SilwoodWeatherDaily %>% mutate(year = format(as.Date(TIMESTAMP, format = "%d/%m/%Y"), "%Y"), julian_day = format(as.Date(TIMESTAMP, format = "%d/%m/%y"), "%j"))

budburst_data <- budburst_data %>% mutate(year = format(as.Date(date, format = "%d/%m/%Y"), "%Y"), julian_day = format(as.Date(date, format = "%d/%m/%y"), "%j"))


budburst_final <- merge(budburst_data, SilwoodWeatherDaily, by = c("julian_day", "year"))
budburst_final <- budburst_final %>% filter(species == "quercus.robur") %>% filter((score %in% c("2"))) 
budburst_final <- budburst_final %>% group_by(TreeID, year) %>% slice_min(order_by = julian_day, n = 1) %>% ungroup()
budburst_final <- budburst_final %>%
  filter(note != "dead")


SilwoodWeatherDaily <- SilwoodWeatherDaily %>%
  mutate(mean_temp = Air_Temp..Deg.C...Smp.) %>%
  group_by(year)

SilwoodWeatherDaily <- SilwoodWeatherDaily %>%
    arrange(julian_day) %>%
    group_by(year) %>%
    mutate(
    mean_temp_14d = rollmean(mean_temp, k = 14, fill = NA, align = "right")
) %>%
    ungroup()

budburst_env <- merge(
    budburst_final,
    SilwoodWeatherDaily[, c("julian_day", "year", "mean_temp", "mean_temp_14d")],
    by = c("julian_day", "year"),
    all.x = TRUE
)

budburst_env$TreeID <- as.factor(budburst_env$TreeID)
budburst_env$julian_day <- as.numeric(budburst_env$julian_day)

M1 <- lmer(formula = julian_day ~ scale(mean_temp_14d) + (1 | TreeID) + (1 | year), data = budburst_env)
summary(M1)

par(mfrow = c(2,2))
plot(budburst_env$julian_day)
