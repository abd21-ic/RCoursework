# Preamble
rm(list=ls())
d <- read.table("../data/SparrowSize.txt", header=TRUE)
str(d)

#boxplots
boxplot(d$Mass~d$Sex.1, col = c("red", "blue"), ylab="Body mass (g)")
t.test1 <- t.test(d$Mass~d$Sex.1)
t.test1

d1<-as.data.frame(head(d, 50))
length(d1$Mass)

#exercises
#1.1 - Sex & Wing in 2001
d2001<-subset(d, d$Year==2001)
t.test1.1 <- t.test(d2001$Wing~d2001$Sex.1)
t.test1.1
#1.2 - Sex & Wing in total
t.test1.2 <- t.test(d$Wing~d$Sex.1)
t.test1.2
#1.3 - Sex & Tarsus in total
t.test1.3 <- t.test(d$Tarsus~d$Sex.1)
t.test1.3

extract_ttest <- function(ttest_obj) {
  means <- ttest_obj$estimate
  data.frame(
    Female_Mean = means[1],
    Male_Mean   = means[2],
    p_value     = ttest_obj$p.value,
    row.names = NULL
  )
}

results <- rbind(
  cbind(Comparison = "Wing (2001)", extract_ttest(t.test1.1)),
  cbind(Comparison = "Wing (Full Dataset)", extract_ttest(t.test1.2)),
  cbind(Comparison = "Tarsus (Full Dataset)", extract_ttest(t.test1.3))
)

results

#install.packages("tibble")
library(tibble)
library(dplyr)

extract_ttest <- function(ttest_obj) {
  means <- ttest_obj$estimate
  tibble(
    Female_Mean = means[1],
    Male_Mean   = means[2],
    p_value     = ttest_obj$p.value
  )
}

results_tbl <- rbind(
  cbind(Comparison = "Wing (2001)", extract_ttest(t.test1.1)),
  cbind(Comparison = "Wing (Full Dataset)", extract_ttest(t.test1.2)),
  cbind(Comparison = "Tarsus (Full Dataset)", extract_ttest(t.test1.3))
)

results_tbl <- results_tbl %>%
  mutate(
    Female_Mean = round(Female_Mean),
    Male_Mean   = round(Male_Mean),
    p_value     = signif(p_value)
  )

results_tbl
