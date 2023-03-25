library(dplyr)
library(ggplot2)
#library(GGally)
library(MASS)
library(car)
library(Hmisc)
library(knitr)

library(ggfortify)
### Assignment1
# Reading in the data
data<- read.table("../02_Assignment2/earinfect.txt",header=T)
names(data)
# first step -> descriptive analytics of the dependent variable

data <- mutate_if(data, is.character, as.factor)

fit1 <- glm(infections ~ swimmer + location + age + sex, family = poisson, data = data)
fit1.offset<-glm(infections ~ offset(log(persons))+1 + swimmer + location + age + sex, family = poisson, data = data)
summary(fit1)

summary(fit1.offset)
anova(fit1.offset, test="Chisq")
