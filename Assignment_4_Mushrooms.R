library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(fitdistrplus)
library(MASS)
library(lme4)
library(modelr)

getwd()
setwd("C://Users/Shilo/Desktop/Course_Materials/Data_Course/data/")
df = read.csv("mushroom_growth.csv")

plot(fitdist(df$GrowthRate, distr = "norm"))
plot(fitdist(df$GrowthRate, distr = "lnorm"))
plot(fitdist(df$GrowthRate, distr = "logis"))
plot(fitdist(df$GrowthRate, distr = "gamma"))

plot(fitdist(log10(df$GrowthRate), distr = "norm")) 

df$log10_GrowthRate = log10(df$GrowthRate)



summary(df)

mod1 = aov(GrowthRate ~ (Humidity+Nitrogen+Light)*Species, data = df)
mod2 = aov(GrowthRate ~ (Humidity*Nitrogen*Light)*Species, data = df)

summary(mod1)
summary(mod2)

TukeyHSD(mod1)
TukeyHSD(mod2)

anova(mod1, mod2)


# exploratory plots
plot1 = ggplot(mod1, mapping = aes(x=Light, y=GrowthRate, col = Species))
plot1 + geom_point() + geom_smooth()

plot2 = ggplot(df, mapping = aes(x=Humidity, y=GrowthRate, col = Species))
plot2 + geom_point() + geom_smooth()

plot3 = ggplot(mod2, mapping = aes(x=Light, y=GrowthRate, col = Species)) 
plot3 + geom_point() + geom_smooth()

plot4 = ggplot(mod2, mapping = aes(x=Light, y=GrowthRate, col = Species))

#Species:Light, Species:Nitrogen, Species:Temperature, Species:Humidity, Light:Humidity, Temperature:Humidity, Species:Light:Humidity

ggplot(df, mapping = aes(x= Temperature, y= GrowthRate, col = Species)) +
  geom_point() + geom_smooth(method = "aov") + 
  ggtitle("Growth as a function of Temperature") + 
  labs(subtitle = "Color by Species")

boxplot(df$GrowthRate ~ df$Species*df$Temperature*df$Humidity)

#predictive models
model_1 = df %>% add_predictions(model = mod1) 
mod_aov = aov(GrowthRate ~ (Humidity+Nitrogen+Light+Species)*pred, data = model_1)

model_df2 = df %>% add_predictions(model = mod2)
mod_aov2 = aov(GrowthRate ~ (Humidity*Nitrogen*Light*Species)*pred, data = model_df2)

summary(mod_aov) 
summary(mod_aov2) 

TukeyHSD(mod_aov)
anova(mod_aov,mod_aov2) #tells me that pred model mod_aov2 is sig dif from pred model mod_aov

for(h in names(model_1)){
  plot(model_1[,"GrowthRate"] ~ model_1[,h], xlab = h, ylab = "GrowthRate", main = h)}

ggplot(data = model_1) +
  geom_point(aes(x=Nitrogen, y=GrowthRate, col = Species)) +
  geom_point(aes(x=pred, y=GrowthRate, col=Species)) +
  geom_smooth(aes(x=Nitrogen, y=GrowthRate),col = "Black") +
  geom_smooth(aes(pred,GrowthRate,col = Species))

ggplot(data = model_1) +
  geom_point(aes(x=Light, y=GrowthRate, col = Species)) +
  geom_point(aes(x=pred, y=GrowthRate, col=Species)) +
  geom_smooth(aes(x=pred, y=GrowthRate),col = "Black")

ggplot(data = model_df2) +
  geom_point(aes(x=Light, y=GrowthRate, col = Species)) +
  geom_point(aes(x=pred, y=GrowthRate, col=Species)) +
  geom_smooth(aes(x=pred, y=GrowthRate),col = "Black")

ggplot(model_df2) + 
  geom_point(aes(x=pred, y=GrowthRate, col=Species, alpha = 0.5)) +
  geom_smooth(aes(x=pred, y=GrowthRate),col = "Black")
