require(ggplot2)
require(MASS)
require(ggpubr)

getwd()
fish<- read.csv("../data/fisheries.csv", stringsAsFactors = T)
str(fish)

ggplot(fish, aes(x=MeanDepth, y=TotAbund))+
  geom_point()+
  labs(x= "Mean Depth (km)", y="Total Abundance")+
  theme_classic()

M1<- glm(TotAbund~MeanDepth, data = fish, family = "poisson")
summary(M1)

par(mfrow=c(2,2))
plot(M1)

sum(cooks.distance(M1)>1)

scatterplot<-ggplot(fish, aes(x=MeanDepth, y=TotAbund, color=factor(Period)))+
  geom_point()+
  labs(x= "Mean Depth (km)", y="Total Abundance")+
  theme_classic()+
  scale_color_discrete(name="Period", labels=c("1979-1989", "1997-2002"))
boxplot<- ggplot(fish, aes(x=factor(Period, labels=c("1979-1989", "1997-2002")), y=TotAbund))+
  geom_boxplot()+
  theme_classic()+
  labs(x="Period", y="Total Abundance")
ggarrange(scatterplot, boxplot, labels=c("A","B"), ncol=1, nrow=2)

fish$Period<- factor(fish$Period)
M2<- glm(TotAbund~MeanDepth*Period, data = fish, family="poisson")
summary(M2)

anova(M2, test="Chisq")

M3<- glm.nb(TotAbund~MeanDepth*Period, data = fish)
summary(M3)

anova(M3, test = "Chisq")

M4<- glm.nb(TotAbund~MeanDepth+Period, data = fish)
summary(M4)

anova(M4, test = "Chisq")

par(mfrow=c(2,2))
plot(M4)

range(fish$MeanDepth)
period1 <- data.frame(MeanDepth=seq(from=0.804, to=4.865, length=100), Period="1")
period2 <- data.frame(MeanDepth=seq(from=0.804, to=4.865, length=100), Period="2")
period1_predictions<- predict(M4, newdata = period1, type = "link", se.fit = TRUE) # the type="link" here predicted the fit and se on the log-linear scale. 
period2_predictions<- predict(M4, newdata = period2, type = "link", se.fit = TRUE)
period1$pred<- period1_predictions$fit
period1$se<- period1_predictions$se.fit
period1$upperCI<- period1$pred+(period1$se*1.96)
period1$lowerCI<- period1$pred-(period1$se*1.96)
period2$pred<- period2_predictions$fit
period2$se<- period2_predictions$se.fit
period2$upperCI<- period2$pred+(period2$se*1.96)
period2$lowerCI<- period2$pred-(period2$se*1.96)
complete<- rbind(period1, period2)

ggplot(complete, aes(x=MeanDepth, y=exp(pred)))+
  geom_line(aes(color=factor(Period)))+
  geom_ribbon(aes(ymin=exp(lowerCI), ymax=exp(upperCI), fill=factor(Period), alpha=0.3), show.legend = FALSE)+ 
  geom_point(fish, mapping = aes(x=MeanDepth, y=TotAbund, color=factor(Period)))+
  labs(y="Total Abundance", x="Mean Depth (km)")+
  theme_classic()+
  scale_color_discrete(name="Period", labels=c("1979-1989", "1997-2002"))

install.packages("ggeffects")
require(ggeffects)
plot(ggpredict(M4, terms=c("MeanDepth", "Period")), show_data=T)

mites<- read.csv("../data/bee_mites.csv")
mites_m1<- glm(Dead_mites~Concentration, data = mites, family = "poisson")
summary(mites_m1)
anova(mites_m1, test = "Chisq")

par(mfrow=c(2,2)) #partitioning the plot window into a 2X2
plot(mites_m1)

range(mites$Concentration) # Finding the range of concentration
## [1] 0.00 2.16
new_data <- data.frame(Concentration=seq(from=0, to=2.16, length=100))
predictions<- predict(mites_m1, newdata = new_data, type = "link", se.fit = TRUE) # the type="link" here predicted the fit and se on the log-linear scale. 
new_data$pred<- predictions$fit
new_data$se<- predictions$se.fit
new_data$upperCI<- new_data$pred+(new_data$se*1.96)
new_data$lowerCI<- new_data$pred-(new_data$se*1.96)

# Making the Plot 
ggplot(new_data, aes(x=Concentration, y=exp(pred)))+ 
  geom_line(col="black")+
  geom_ribbon(aes(ymin=exp(lowerCI), ymax=exp(upperCI), alpha=0.1), show.legend = FALSE, fill="grey")+ 
  geom_point(mites, mapping = aes(x=Concentration, y=Dead_mites), col="blue")+
  labs(y="Number of Dead Mites", x="Concentration (g/l)")+
  theme_classic()