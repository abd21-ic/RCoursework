# Preamble
rm(list=ls())
d <- read.table("../data/SparrowSize.txt", header=TRUE)
str(d)

# Bird ID & Year
d$BirdIDFact<-as.factor(d$BirdID)
str(d$BirdIDFact)
plot(d$Mass~as.factor(d$Year), xlab="Year", ylab="House sparrow body mass(g)")
plot(d$Mass~d$Year, xlab="Year", ylab="House sparrow body mass(g)")
dev.off()

# Blue Tits Data
rm(list=ls())
b <- read.table("../data/BTLD.txt", header = T)
str(b)

mean(b$ClutchsizeAge7, na.rm = TRUE)
plot(b$LD.in_AprilDays.~jitter(b$Year), ylab="Laying date (April days)", xlab="Year", pch=19, cex=0.3)

install.packages("ggplot2")
require(ggplot2)

# Violin plot
p <- ggplot(b, aes(x=Year, y=LD.in_AprilDays.)) +
geom_violin()
p
dev.off()
boxplot(b$LD.in_AprilDays.~b$Year, ylab="Laying date (April days)", xlab="Year")
dev.off()
p <- ggplot(b, aes(x=as.factor(Year), y=LD.in_AprilDays.)) +
geom_violin()
p
p + stat_summary(fun.data="mean_sdl",
geom="pointrange")
dev.off()
