### Assignment1

dioxin<-read.csv("../01_Assignment1/dioxin.csv",header=T)
attach(dioxin)
names(dioxin)

pairs(dioxin,panel=panel.smooth)


cov(dioxin)
cor(dioxin)
library(corrplot)
corrplot(cor(dioxin))