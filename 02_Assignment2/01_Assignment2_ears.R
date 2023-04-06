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
fit1.offset<-glm(infections ~ offset(log(persons)) + swimmer + location + age + sex, family = poisson(link="log"), data = data)
summary(fit1)

hist(data$infections)

#summary(glm(infections ~ swimmer + location + age + sex+log(persons), family = poisson, data = data))
summary(fit1.offset)
anova(fit1.offset, test="Chisq")
Anova(fit1.offset, type="III")

# Goodness of fit
pchisq(fit1.offset$deviance, fit1.offset$df.residual)
pchisq(fit1.offset$null.deviance, fit1.offset$df.null)
anova(glm(infections ~ offset(log(persons))+1, family = poisson, data = data),fit1.offset, test="Chisq")
summary(glm(infections ~ offset(log(persons))+1, family = poisson, data = data))

drop1(fit1.offset, test="Chisq")
fit1.offred <- step(fit1.offset)
fit1.offred

par(mfrow=c(2,2))
plot(fit1)
plot(fit1.offset)

anova(fit1.offset, (glm(infections ~ offset(log(persons)) + swimmer + location + sex, family = poisson, data = data)), test="Chisq")
fit1.offloc <- glm(infections ~ offset(log(persons)) +location, family = poisson, data = data)
summary(fit1.offloc)

pchisq(fit1.offloc$deviance, fit1.offloc$df.residual)
anova(glm(infections ~ offset(log(persons))+1, family = poisson, data = data),fit1.offloc, test="Chisq")
anova(fit1.offloc,fit1.offset, test="Chisq")


fit.qp <- (glm(infections ~ offset(log(persons)) + swimmer + location + age + sex, family = quasipoisson(link="log"), data = data))
summary(fit.qp)
plot(fit.qp)

fit.qps <- glm(infections ~ offset(log(persons))+ location, family = quasipoisson(link="log"), data = data)
summary(fit.qps)
anova(fit.qp,fit.qps, test="F")
logLik(fit1.offset)

logLik(fit.qps)
plot(fit.qp)
#summary(glm.nb(infections ~ offset(log(persons)) + swimmer + location + age + sex, data = data))


##-
# mean(data$infections)
# var(data$infections)
# 
# mean(data$infections/data$persons)
# var(data$infections/data$persons)
# sum(data$persons)
# mean(data$infections/data$persons*mean(data$infections))
# var(data$infections/data$persons*mean(data$infections))

#data$infections/data$persons*mean(data$infections)



#install.packages("COMPoissionReg")
#library("COMPoissionReg")
