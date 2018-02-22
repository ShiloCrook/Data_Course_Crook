rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(fitdistrplus)
library(MASS)
library(lme4)


df = CO2

#Quickplot
plot(df)

#Every variable as a function of uptake
for(i in names(df)){
  plot(df[,"uptake"] ~ df[,i], xlab = i, ylab = "uptake", main = i)
}

#Frequency of uptake values
hist((df$uptake), xlab = "uptake", main = "uptake")

#Density of uptake values
plot(density(df$uptake), xlab = "uptake", main = "uptake")

plot(fitdist(df$uptake, "norm"))

uptake_model = aov(uptake ~ conc, data = df)
summary(uptake_model)


ggplot(df, mapping = aes(x=conc, y=uptake, fill = Type)) +
  geom_boxplot()
