rm(list=ls())

# Importing Data
d <- read.table("../data/SparrowSize.txt", header=TRUE)
str(d)

# Exploring Sparrow Data
length(d$Tarsus)
hist(d$Tarsus)
dev.off()
mean(d$Tarsus, na.rm = T)
median(d$Tarsus, na.rm = T)
mode(d$Tarsus)

# Histograms of Tarsus
par(mfrow = c(2, 2))
hist(d$Tarsus, breaks = 3, col="grey")
hist(d$Tarsus, breaks = 10, col="grey")
hist(d$Tarsus, breaks = 30, col="grey")
hist(d$Tarsus, breaks = 100, col="grey")
dev.off()

head(table(d$Tarsus))

# Rounding Tarsus to 1 decimal place
d$Tarsus.rounded<-round(d$Tarsus, digits=1)
head(d$Tarsus.rounded)

# install.packages("dplyr")
require(dplyr)

# Frequency table of Tarsus values (removing NA values)
d2<-subset(d, d$Tarsus!="NA")
length(d$Tarsus)-length(d2$Tarsus)
TarsusTally <- d2 %>% count(Tarsus.rounded, sort=TRUE)
TarsusTally
TarsusTally[[1]][1] # Most common Tarsus value

# Summary statistics of Tarsus
range(d$Tarsus, na.rm=TRUE)
var(d$Tarsus, na.rm=TRUE)
sd(d$Tarsus, na.rm=TRUE)

# z-score comes from standardised normal distrbution, mean = 0, sd = 1
zTarsus <- (d2$Tarsus - mean(d2$Tarsus))/sd(d2$Tarsus)
hist(zTarsus)
znormal <- rnorm(1e+06)
hist(znormal, breaks = 100)
summary(znormal)
qnorm(c(0.025, 0.975))
pnorm(.Last.value)

# More histograms
par(mfrow = c(1, 2))
hist(znormal, breaks = 100)
abline(v = qnorm(c(0.25, 0.5, 0.75)), lwd = 2)
abline(v = qnorm(c(0.025, 0.975)), lwd = 2, lty = "dashed")
plot(density(znormal))
abline(v = qnorm(c(0.25, 0.5, 0.75)), col = "gray")
abline(v = qnorm(c(0.025, 0.975)), lty = "dotted", col = "black")
abline(h = 0, lwd = 3, col = "blue")
text(2, 0.3, "1.96", col = "red", adj = 0)
text(-2, 0.3,"-1.96", col = "red", adj = 1)
dev.off()

#boxplots
boxplot(d$Tarsus~d$Sex.1, col = c("red", "blue"), ylab="Tarsus length (mm)")
dev.off()
