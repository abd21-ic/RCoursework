rm(list=ls())
d<-read.table("../data/SparrowSize.txt", header=TRUE)
plot(d$Mass~d$Tarsus, ylab="Mass (g)", xlab="Tarsus (mm)", pch=19, cex=0.4)

x<-c(1:100)
a<-0.5
b<-1.5
y<-b*x+a
plot(x,y, xlim=c(0,100), ylim=c(0,100), pch=19, cex=0.5)

head(d$Mass)

d$Mass[1]

length(d$Mass)
