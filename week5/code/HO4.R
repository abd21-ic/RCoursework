# Preamble
rm(list=ls())
d <- read.table("../data/SparrowSize.txt", header=TRUE)
str(d)

# Standard Error
d <- read.table("../data/SparrowSize.txt", header=TRUE)
d1<-subset(d, d$Tarsus!="NA")
seTarsus<-sqrt(var(d1$Tarsus)/length(d1$Tarsus))
seTarsus

# SE for 2001
d12001<-subset(d1, d1$Year==2001)
seTarsus2001<-sqrt(var(d12001$Tarsus)/length(d12001$Tarsus))
seTarsus2001

#Setting a random tail test and performing test statistics
rm(list=ls())
TailLength <-rnorm(500,mean=3.8, sd=2)
summary(TailLength)
var(TailLength)
sd(TailLength)
hist(TailLength)
dev.off()

x<-1:length(TailLength)
y<-mean(TailLength)+0*x
min(TailLength)
max(TailLength)
plot(x,y, cex=0.03, ylim=c(2,5),xlim=c(0,500), xlab="Sample size n", ylab="Mean of tail length ±SE (m)", col="red")
SE<-c(1)
mu <- c(1)
for (n in 1:length(TailLength)) {
    d<-sample(TailLength, n, replace=FALSE)
    mu[n]<-mean(TailLength)
    SE[n]<-sd(TailLength)/sqrt(n)
}
head(SE)
head(mu)
up<-mu+SE
down<-mu-SE
x<-1:length(SE)
segments(x, up, x1=x, y1=down, lty=1)

rm(list=ls())
TailLength<-rnorm(201,mean=3.8, sd=2)
length(TailLength)

x<-1:201
y<-mean(TailLength)+0*x
plot(x,y, cex=0.03, ylim=c(3,4.5),xlim=c(0,201), xlab="Sample size n", ylab="Mean of tail length ±SE (m)", col="red")
n<-seq(from=1, to=201, by=10)
n
SE<-c(1)
mu<-c(1)
for (i in 1:length(TailLength)) {
    d<-sample(TailLength, n[i], replace=FALSE)
    mu[i]<-mean(TailLength)
    SE[i]<-sd(TailLength)/sqrt(n[i])
}
up<-mu+SE
down<-mu-SE
length(up)

plot(x,y, cex=0.03, ylim=c(3,4.5),xlim=c(0,201), xlab="Sample size n", ylab="Mean of tail length ±SE (m)", col="red")
points(n,mu,cex=0.3, col="red")
segments(n, up, x1=n, y1=down, lty=1)


# Exercises
rm(list=ls())
d <- read.table("../data/SparrowSize.txt", header=TRUE)
d1<-subset(d, d$Tarsus!="NA" & d$Mass!="NA" & d$Wing!="NA" & d$Bill!="NA")

#SE errors
seTarsus<-sqrt(var(d1$Tarsus)/length(d1$Tarsus))
seTarsus
seMass<-sqrt(var(d1$Mass)/length(d1$Mass))
seMass
seWing<-sqrt(var(d1$Wing)/length(d1$Wing))
seWing
seBill<-sqrt(var(d1$Bill)/length(d1$Bill))
seBill

d2001<-subset(d, d$Year==2001)
se <- function(x) {
  sqrt(var(x, na.rm = TRUE) / sum(!is.na(x)))
}

se_Tarsus <- se(d2001$Tarsus)
se_Tarsus
se_Mass   <- se(d2001$Mass)
se_Mass
se_Wing   <- se(d2001$Wing)
se_Wing
se_Bill   <- se(d2001$Bill)
se_Bill

#No 2001 bird has bill length recorded

# Confidence Interval

ci95 <- function(x) {
  m  <- mean(x, na.rm = TRUE)
  s  <- se(x)
  n  <- sum(!is.na(x))
  t_crit <- qt(0.95, df = n - 1)
  lower <- m - t_crit * s
  upper <- m + t_crit * s
  c(mean = m, lower_CI = lower, upper_CI = upper)
}

results <- sapply(d2001[, c("Tarsus", "Mass", "Wing")], ci95)
results <- as.data.frame(t(results))
results
