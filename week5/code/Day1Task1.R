library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/Users/macbookpro/Documents/RCoursework/code")

d<-read.table("../data/SparrowSize.txt", header=TRUE)

RepeatsPerBirdYear <- d %>%
  group_by(BirdID, Year) %>%
  summarise(n_repeats = n()) %>%
  arrange(desc(n_repeats))

print(RepeatsPerBirdYear)

write.csv(RepeatsPerBirdYear, file = "../results/RepeatsPerBirdYear.csv", row.names = FALSE)

install.packages("tidyr")
library(tidyr)

IndividualsPerYearSex <- d %>%
  group_by(Year, Sex.1) %>%
  summarise(n_individuals = n_distinct(BirdID)) %>%
  arrange(Year, Sex.1)


print(IndividualsPerYearSex)

IndividualsTable <- IndividualsPerYearSex %>%
  pivot_wider(names_from = Sex.1, values_from = n_individuals, values_fill = 0)

print(IndividualsTable)
write.csv(IndividualsTable, file = "../results/IndividualsPerYearSex.csv", row.names = FALSE)


sparrowcountyear <- RepeatsPerBirdYear %>%
  group_by(Year) %>%
  summarise(n = n(), .groups = "drop")

sparrowcount_bar <- ggplot(sparrowcountyear, aes(x = factor(Year), y = n)) +
  geom_bar(stat = "identity", fill = "#A6CEE3") +
  labs(x = "Year", y = "Number of Captures (Bird-Year combinations)",
       title = "Repeat Captures per Year") +
  theme_classic()

print(sparrowcount_bar)

sparrowsex_bar <- ggplot(IndividualsPerYearSex, aes(x = factor(Year), y = n_individuals, fill = Sex.1)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("male" = "#A6CEE3", "female" = "#FB9A99")) +
  labs(x = "Year", y = "Number of Individuals",
       title = "Number of Individuals Captured per Year by Sex") +
  theme_classic()

print(sparrowsex_bar)
