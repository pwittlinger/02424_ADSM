# Load necessary libraries
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
dev.off()
hist(data$infections, main="Histogram of infections")
mean(data$persons)
var(data$persons)
min(data$persons)
sum(data$persons)
sum(data$infections)

########################
# First we fit a simple model with no interactions, no offset
fit1 <- glm(infections ~ swimmer + location + age + sex, family = poisson, data = data)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)

1-pchisq(fit1$deviance, fit1$df.residual)


# Almost all parameters are significant
# But the model doesn'capture some trend in the data -> Parabola in residual plot

# Now we look at the same model using an offset.
# 
fit1.offset<-glm(infections ~ offset(log(persons)) + swimmer + location + age + sex, family = poisson(link="log"), data = data)
summary(fit1.offset)
plot(fit1.offset)
anova(fit1.offset, test="Chisq")
Anova(fit1.offset, type="II")
Anova(fit1.offset, type="III")
1-pchisq(fit1.offset$deviance, fit1.offset$df.residual)


fit1.offset<-glm(infections ~ offset(log(persons)) + (swimmer + location + age + sex)^2, family = poisson(link="log"), data = data)
1-pchisq(fit1.offset$deviance, fit1.offset$df.residual)
plot(fit1.offset)

summary(fit1.offset)
anova(fit1.offset, test="Chisq")
Anova(fit1.offset, type="II")
Anova(fit1.offset, type="III")

####################
# Goodness of fit tests

# First we fit the null-model (easier to compare later)
H_0 <- glm(infections ~ offset(log(persons))+1, family = poisson, data = data)
summary(H_0) # comparatively high deviance

pchisq(fit1.offset$deviance, fit1.offset$df.residual)
pchisq(fit1.offset$null.deviance, fit1.offset$df.null)

pchisq(H_0$deviance, H_0$df.residual)

anova(H_0,fit1.offset, test="Chisq")




drop1(fit1.offset, test="Chisq")
fit1.offred <- step(fit1.offset)
fit1.offred

par(mfrow=c(2,2))
plot(fit1)
plot(fit1.offset)
plot(fit1.offloc)

anova(fit1.offset, (glm(infections ~ offset(log(persons)) + swimmer + location + sex, family = poisson, data = data)), test="Chisq")
fit1.offloc <- glm(infections ~ offset(log(persons)) +location*sex, family = poisson, data = data)
sofloc <- summary(fit1.offloc)

# print coefficients to latex
#knitr::kable(sofloc$coefficients, "latex")

pchisq(fit1.offloc$deviance, fit1.offloc$df.residual)
anova(glm(infections ~ offset(log(persons))+1, family = poisson, data = data),fit1.offloc, test="Chisq")
anova(fit1.offset,fit1.offloc, test="Chisq")

anova(H_0, fit1.offloc, test="Chisq")
pchisq(fit1.offloc$deviance, fit.offloc$df.residual)


fit_step <- step(fit1.offset)

anova(fit_step, fit_fin)

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

#fit.qp$


knitr::kable(anova(fit.q0, fit.qp, test="F"), "latex")

fit.qps <- glm(infections ~ offset(log(persons))+ location*sex, family = quasipoisson(link="log"), data = data)
summary(fit.qps)
anova(fit.qp,fit.qps, test="F")
anova(fit.q0, fit.qps, test="F")

fit.qps <- glm(infections ~ offset(log(persons))+ location*sex+swimmer, family = quasipoisson(link="log"), data = data)

Anova(fit.qps, type="II", test="F")
Anova(fit.qps, type="III", test="F")
anova(fit.qps, test="F")
anovaTests(fit.qps)

summary(fit.qps)

sofloc <- summary(fit.qps)

# print coefficients to latex
knitr::kable(sofloc$coefficients, "latex")
# knitr::kable(sofloc$coefficients, "latex")



logLik(fit1.offset)

logLik(fit.qps)
plot(fit.qps)

fit_fin <- glm(infections ~ offset(log(persons))+ location*sex+swimmer, family = poisson(link="log"), data = data)
sofloc <- summary(fit_fin)

sofloc
knitr::kable(sofloc$coefficients, "latex")
anovaTests(fit_fin, "Chisq")

anova(fit_fin, test="Chisq")
Anova(fit_fin, type="III")


pchisq(fit_fin$deviance, fit_fin$df.residual)
anova(H_0, fit_fin, test="Chisq")
anova(fit_fin, fit1.offloc, test="Chisq")
anova(fit1.offset, fit_fin, test="Chisq")

exp(mean(log(data$infections/data$persons)))
mean(data$infections/data$persons)

fit_fin$coefficients
exp(fit_fin$coefficients)
summary(fit_fin)

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


anovaTests <- function(modelObject, testString="F"){
  # perform type I, II and III Anova on the given model with the corresponding test

  if(testString=="F"){
    print(anova(modelObject))
    print("######################")
    print(Anova(modelObject, type="II", test=testString))
    print("######################")
    Anova(modelObject, type="III", test=testString)
  }
  else{
    print(anova(modelObject,test=testString))
    print("######################")
    print(Anova(modelObject, type="II"))
    print("######################")
    print(Anova(modelObject, type="III"))
  }
}

#######################################
# Iterative testing
# fit.qp is a quasi-poisson with full 2-way interactions
drop1(fit.qp, test="F")
qpdrop1 <- glm(infections ~ offset(log(persons))+ (location+sex+age)^2+(swimmer+location+age)^2, family = quasipoisson(link="log"), data = data)



summary(qpdrop1)
summary(fit.qp)
