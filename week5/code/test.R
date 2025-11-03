rm(list=ls())

getwd()
setwd("/Users/macbookpro/Documents/RCoursework/code")

myNumericVector <- c(1.3,2.5,1.9,3.4,5.6,1.4,3.1,2.9)
myCharacterVector <- c("low","low","low","low","high","high","high","high")
myLogicalVector <- c(TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE)

str(myNumericVector)
str(myCharacterVector)
str(myLogicalVector)

myMixedVector <-c(1, TRUE, FALSE, 3, "help", 1.2, TRUE, "notwhatIplanned")
str(myMixedVector)

install.packages("lme4")
library(lme4)
require(lme4)

d<-read.table("../data/SparrowSize.txt", header=TRUE)
str(d)

require(dplyr)

BirdIDCount <- d %>% count(BirdID,BirdID, sort=TRUE)
BirdIDCount %>% count(n)

table(d$BirdID)

table(d$Year)

table(table(d$BirdID))

table(d$Year)

type_candidates <- intersect(c("Type","Species","Sex","Morph","Age"), names(d))
if(length(type_candidates) == 0){
  type_candidates <- names(d)[sapply(d, function(x) is.character(x) || is.factor(x))]
  type_candidates <- setdiff(type_candidates, c("BirdID","Year"))
}

if(length(type_candidates) == 0){
  stop("No candidate 'type' column found. Add a column name (e.g. 'Species' or 'Sex') to compute repeats by type.")
}

results_list <- list()
for(tc in type_candidates){
  by_bird_year_type <- d %>%
    group_by(Year, Type = .data[[tc]], BirdID) %>%
    summarize(obs = n(), .groups = "drop")
  summary_tbl <- by_bird_year_type %>%
    group_by(Year, Type) %>%
    summarize(
      distinct_birds = n(),                        # distinct BirdIDs for that Year+Type
      birds_with_repeats = sum(obs > 1),           # how many birds seen >1 time that Year+Type
      total_repeat_records = sum(pmax(0, obs - 1)),# extra observations beyond first
      total_records = sum(obs),
      .groups = "drop"
    ) %>%
    arrange(Year, Type)
  results_list[[tc]] <- summary_tbl
  cat("\nRepeats summary by Year and", tc, ":\n")
  print("../results/ResultsTask1.csv")
}
