rm(list=ls())
d<-read.table("../../week5/data/SparrowSize.txt", header=TRUE)
str(d)

hist(d$Tarsus, main="", xlab="Sparrow tarsus length (mm)", col="grey")

hist(d$Tarsus, main="", xlab="Sparrow tarsus length (mm)", col="grey",
prob=TRUE) # this argument tells R to plot density instead of frequency,
lines(density(d$Tarsus,na.rm=TRUE), # density plot
lwd = 2)
abline(v = mean(d$Tarsus, na.rm = TRUE), col = "red",lwd = 2)
abline(v = mean(d$Tarsus, na.rm = TRUE)-sd(d$Tarsus, na.rm = TRUE), col = "blue",lwd = 2, lty=5)
abline(v = mean(d$Tarsus, na.rm = TRUE)+sd(d$Tarsus, na.rm = TRUE), col = "blue",lwd = 2, lty=5)

par(mfrow=c(2,1))
hist(d$Tarsus[d$Sex==1], main="", xlab="Male sparrow tarsus length (mm)", col="grey", prob=TRUE)
lines(density(d$Tarsus[d$Sex==1],na.rm=TRUE), lwd = 2)
abline(v = mean(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "red",lwd = 2)
abline(v = mean(d$Tarsus[d$Sex==1], na.rm = TRUE)-sd(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "blue",lwd = 2, lty=5)
abline(v = mean(d$Tarsus[d$Sex==1], na.rm = TRUE)+sd(d$Tarsus[d$Sex==1], na.rm = TRUE), col = "blue",lwd = 2, lty=5)
hist(d$Tarsus[d$Sex==0], main="", xlab="Female sparrow tarsus length (mm)", col="grey", prob=TRUE)
lines(density(d$Tarsus[d$Sex==0],na.rm=TRUE), lwd = 2)
abline(v = mean(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "red",lwd = 2)
abline(v = mean(d$Tarsus[d$Sex==0], na.rm = TRUE)-sd(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "blue",lwd = 2, lty=5)
abline(v = mean(d$Tarsus[d$Sex==0], na.rm = TRUE)+sd(d$Tarsus[d$Sex==0], na.rm = TRUE), col = "blue",lwd = 2, lty=5)
dev.off()

d1<-subset(d, d$Tarsus!="NA")
d1<-subset(d1, d1$Wing!="NA")
sumz<-var(d1$Tarsus)+var(d1$Wing)
test<-var(d1$Tarsus+d1$Wing)
sumz

plot(jitter(d1$Wing),d1$Tarsus, pch=19, cex=0.4)
dev.off()

sumz<-var(d1$Tarsus)+var(d1$Wing)+2*cov(d1$Tarsus,d1$Wing)
test<-var(d1$Tarsus+d1$Wing)
sumz

uni<-read.table("../data/RUnicorns.txt", header=T)
str(uni)

plot(uni$Bodymass~uni$Hornlength, pch=19, xlab="Unicorn horn length", ylab="Unicorn body mass", col="blue")
mod<-lm(uni$Bodymass~uni$Hornlength)
abline(mod, col="red")
res <- signif(residuals(mod), 5)
pre <- predict(mod)
segments(uni$Hornlength, uni$Bodymass, uni$Hornlength, pre, col="black")

hist(uni$Bodymass)
hist(uni$Hornlength)
hist(uni$Height)

par(mfrow=c(2,1))
boxplot(uni$Bodymass~uni$Pregnant)
plot(uni$Hornlength[uni$Pregnant==0],uni$Bodymass[uni$Pregnant==0], pch=19, xlab="Horn length", ylab="Body mass", xlim=c(2,10), ylim=c(6,19))
points(uni$Hornlength[uni$Pregnant==1],uni$Bodymass[uni$Pregnant==1], pch=19,col="red")
dev.off()

boxplot(uni$Bodymass~uni$Pregnant)
dev.off()

plot(uni$Hornlength[uni$Gender=="Female"],uni$Bodymass[uni$Gender=="Female"],pch=19, xlab="Horn length", ylab="Body mass", xlim=c(2,10), ylim=c(6,19))
points(uni$Hornlength[uni$Gender=="Male"],uni$Bodymass[uni$Gender=="Male"],pch=19, col="red")
points(uni$Hornlength[uni$Gender=="Undecided"],uni$Bodymass[uni$Gender=="Undecided"],pch=19, col="green")

boxplot(uni$Bodymass~uni$Glizz)

u1<-subset(uni, uni$Pregnant==0)
FullModel<-lm(u1$Bodymass~u1$Hornlength+u1$Gender+u1$Glizz)
summary(FullModel)

ModForPlot<-lm(u1$Bodymass~u1$Hornlength)
summary(ModForPlot)

plot(u1$Hornlength[u1$Glizz==0],u1$Bodymass[u1$Glizz==0], pch=19, xlab="Hornlength", ylab="Body mass", xlim=c(2,10), ylim=c(6,19))
points(u1$Hornlength[u1$Glizz==1],u1$Bodymass[u1$Glizz==1], pch=19, col="red")
abline(ModForPlot)

boxplot(u1$Hornlength~u1$Glizz)
