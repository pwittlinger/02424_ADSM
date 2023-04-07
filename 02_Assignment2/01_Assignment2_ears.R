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

hist(data$infections)
mean(data$persons)
var(data$persons)
min(data$persons)
max(data$persons)

########################
# First we fit a simple model
fit1 <- glm(infections ~ swimmer + location + age + sex, family = poisson, data = data)
summary(fit1)
# Almost all parameters are significant

# Now we look at the same model using an offset.
# 
#fit1.offset<-glm(infections ~ offset(log(persons)) + swimmer + location + age + sex, family = poisson(link="log"), data = data)

fit1.offset<-glm(infections ~ offset(log(persons)) + (swimmer + location + age + sex)^2, family = poisson(link="log"), data = data)

summary(fit1.offset)
anova(fit1.offset, test="Chisq")
Anova(fit1.offset, type="II")
Anova(fit1.offset, type="III")


# Goodness of fit
H_0 <- glm(infections ~ offset(log(persons))+1, family = poisson, data = data)
summary(H_0)

1-pchisq(fit1.offset$deviance, fit1.offset$df.residual)
1-pchisq(fit1.offset$null.deviance, fit1.offset$df.null)
anova(H_0,fit1.offset, test="Chisq")




drop1(fit1.offset, test="Chisq")
fit1.offred <- step(fit1.offset)
fit1.offred

par(mfrow=c(2,2))
plot(fit1)
plot(fit1.offset)

anova(fit1.offset, (glm(infections ~ offset(log(persons)) + swimmer + location + sex, family = poisson, data = data)), test="Chisq")
fit1.offloc <- glm(infections ~ offset(log(persons)) +location*sex, family = poisson, data = data)
summary(fit1.offloc)

pchisq(fit1.offloc$deviance, fit1.offloc$df.residual)
anova(glm(infections ~ offset(log(persons))+1, family = poisson, data = data),fit1.offloc, test="Chisq")
anova(fit1.offloc,fit1.offset, test="Chisq")

# Null hypothesis on quasi poisson
fit.q0 <- glm(infections ~ offset(log(persons)), family = quasipoisson(link="log"), data = data)
summary(fit.q0)


# Now a full model on the quasi poisson
#fit.qp <- (glm(infections ~ offset(log(persons)) + swimmer + location + age + sex, family = quasipoisson(link="log"), data = data))
fit.qp <- (glm(infections ~ offset(log(persons)) + (swimmer + location + age + sex)^2, family = quasipoisson(link="log"), data = data))
summary(fit.qp)
Anova(fit.qp, test="F", type="II")
Anova(fit.qp, test="F", type="III")
anova(fit.qp, test="F")
plot(fit.qp)

anova(fit.q0, fit.qp, test="F")

fit.qps <- glm(infections ~ offset(log(persons))+ location*sex, family = quasipoisson(link="log"), data = data)
summary(fit.qps)
anova(fit.qp,fit.qps, test="F")
anova(fit.q0, fit.qps, test="F")

fit.qps <- glm(infections ~ offset(log(persons))+ location*sex+swimmer, family = quasipoisson(link="log"), data = data)

Anova(fit.qps, type="II", test="F")
Anova(fit.qps, type="III", test="F")
anova(fit.qps, test="F")

logLik(fit1.offset)

logLik(fit.qps)
plot(fit.qps)

fit_fin <- glm(infections ~ offset(log(persons))+ location*sex+swimmer, family = poisson(link="log"), data = data)
summary(fit_fin)
anova(fit_fin, test="Chisq")
Anova(fit_fin, type="III")

#summary(glm.nb(infections ~ offset(log(persons)) + swimmer + location + age + sex, data = data))


##-
# mean(data$infections)
# var(data$infections)
# 
# log((data$infections/data$persons))
# var(data$infections/data$persons)
# sum(data$persons)
# mean(data$infections/data$persons*mean(data$infections))
# var(data$infections/data$persons*mean(data$infections))

#data$infections/data$persons*mean(data$infections)



#install.packages("COMPoissionReg")
#library("COMPoissionReg")
