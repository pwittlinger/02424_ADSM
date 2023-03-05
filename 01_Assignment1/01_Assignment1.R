#install.packages("ggfortify")
#install.packages("ggplot2")
#install.packages("GGally")
library(dplyr)
library(ggplot2)
#library(GGally)
library(MASS)
library(car)

library(ggfortify)
### Assignment1
# Reading in the data
dioxin<-read.csv("../01_Assignment1/dioxin.csv",header=T)
names(dioxin)

# Looking at the raw dependent variable
# we can see that there are quite some outliers
plot(dioxin$DIOX)

# still not normally distributed
# both difference in mean + difference in variance
plot(log(dioxin$DIOX))

# looking at distribution
par(mfrow=c(1,2))
hist(dioxin$DIOX)
hist(log(dioxin$DIOX))

lambda_ <- boxcox(dioxin$DIOX ~ 1)
l <- lambda_$x[which.max(lambda_$y)]



y2 = (dioxin$DIOX^l-1)/l
dioxin$trans <- y2
hist(y2)
qqplot(seq(1:57),y2,)

plot(y2)


# Transforming the variables into factors
dioxin <- mutate_if(dioxin, is.character, as.factor)
dioxin$TIME <- as.factor(dioxin$TIME)

df <- dioxin  %>% dplyr::select(where(is.numeric))
df$TIME <- as.factor(df$TIME)
df$OBSERV <- NULL

pairs(df)


p <- ggplot(dioxin, aes(x=PLANT, y=DIOX, fill=LAB)) + geom_boxplot(outlier.size = 2, notch=FALSE, outlier.shape = 16, outlier.colour = "black")
p


### Q2 - Simple additive model
model1 <- lm(DIOX ~ (PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK), data=dioxin)
model1.1 <- lm(DIOX ~ (PRSEK+LOAD+OXYGEN+LAB+TIME+PLANT), data=dioxin)
# only the order in which the variables enter the model has changed
# if we use type II partitioning, the p-values for the factors are identical
# we don't make use of type III partioning due to aliased coeffients -> not every combination has been observerd
summary(model1)
summary(model1.1)

# Regular anova (type I) -> order in which the variables enter the model is important, sequential testing
anova(model1)
anova(model1.1)

# Type II Anova
Anova(model1.1, type="II")
Anova(model1, type="II")

# Plotting model fits
#autoplot(model1)
png(filename = "q2_full_untransformed.png")
par(mfrow=c(2,2))
plot(model1)
dev.off()
# clear increase in variance on the residuals, 3 observations are very influential and create heavy tails on the QQ-Plot
plot(cooks.distance(model1))
# observation number 13 is a clear outlier. It influences the model significantly (cooks d over .5)

## Transformed model

model1.log <- lm(log(DIOX) ~ (PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK), data=dioxin)
summary(model1.log)

par(mfrow=c(2,2))
plot(model1.log)
dev.off()

model1.bc <- lm(trans ~ (PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK), data=dioxin)
summary(model1.bc)

par(mfrow=c(2,2))
plot(model1.bc)
#dev.off()

Anova(model1.log, type="II")


####

# Using only intercept to specify
step(lm(DIOX~1, data=dioxin), ~PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK, direction="forward")
step(lm(DIOX~1+PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK, data=dioxin), direction="back")

# transformed model
step(lm(log(DIOX)~1, data=dioxin), ~PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK, direction="forward")
#step(lm(log(DIOX)~PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK, data=dioxin), direction="backward")
drop1(lm(log(DIOX)~PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK, data=dioxin))
drop1(lm(log(DIOX)~PLANT+TIME+LAB+OXYGEN+LOAD, data=dioxin))
# we get rid of factor PRSEK. Looking at the type II anova, we can see that it's the only variable with a p-value below the significance level of 5%

model1.red <- lm(log(DIOX)~PLANT+TIME+LAB+OXYGEN+LOAD, data=dioxin)

summary(model1.red)

## Q3 - Simple additive model, but with numerical values
### 

model2.log <- lm(log(DIOX) ~ (PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT), data=dioxin)
summary(model2.log)

par(mfrow=c(2,2))
plot(model2.log)
dev.off()

Anova(model2.log, type="II")


#model2.bc <- lm(trans ~ (PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT), data=dioxin)
#summary(model2.bc)
#par(mfrow=c(2,2))
#plot(model2.bc)

# Using only intercept to specify
step(lm(log(DIOX)~1, data=dioxin), ~PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT, direction="forward")

step(lm(log(DIOX)~PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT, data=dioxin),direction="backward")
# AIC tends to overfit on the models when you have small sample sizes.
# Therefore, we go with the output of the ANOVA and drop the term QRAT
model2.red <- lm(log(DIOX) ~ (PLANT+TIME+LAB+O2COR+NEFFEKT), data=dioxin)
summary(model2.red)

# Q4 - predict on the datapoint
#n <- data.frame(PLANT = c(2), TIME = c(1), LAB = c(1), O2COR = c(0.5), NEFFEKT = c(-0.01))
n <- data.frame(PLANT = c("RENO_N"), TIME = c("1"),
                LAB = c("KK"), O2COR = c(0.5), NEFFEKT = c(-0.01))
pn <- predict(model2.red, n, interval="prediction", level=.95)

exp(pn)

## Does the dioxin emission depend on the active variables

#anova(model2.red, update(model2.red,.~.-PLANT))
model2.noplant <- update(model2.red,.~.-PLANT)
anova(model2.noplant, model2.red)
summary(model2.red)
# Looking at the coefficient of the model, we can see a difference in plants

# plot the residuals of a reduced model (no plant) and perform a boxplot on the residuals
r_a <- cbind(dioxin, residuals(model2.noplant))
ggplot(r_a, aes(x=PLANT, y=residuals(model2.noplant))) + geom_boxplot()
# We can see a difference in residuals, which further highlights the need to include plants



# Q7
### Model with second order interaction
model7 <- lm(log(DIOX) ~ (PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2, data=dioxin)
#alias(model7)
summary(model7)
Anova(model7, type="II")
m7 <- (fit.back <- step(model7, direction="back"))
summary(m7)


#summary(step(lm(log(DIOX)~1, data=dioxin), ~PLANT*TIME*LAB*O2COR*NEFFEKT*QRAT, direction="forward"))
summary(step(lm(log(DIOX)~1, data=dioxin), ~(PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2, direction="forward"))


model7.full <- lm(log(DIOX) ~ ((PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2
                               +QROEG+TOVN+TROEG+POVN+CO2+CO+SO2+HCL+H2O), data=dioxin)

summary(model7.full)

summary(step(model7.full, direction="backward"))

summary(step(lm(log(DIOX)~(PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2, data=dioxin), ~(QROEG+TOVN+TROEG+POVN+CO2+CO+SO2+HCL+H2O), direction="forward"))


summary(step(model2.red, ~(PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2, direction="forward"))

m8 <-step(model2.red, ~(PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2, direction="forward")

model2.red

step(m8, ~.(QROEG+TOVN+TROEG+POVN+CO2+CO+SO2+HCL+H2O), direction="forward")

m8_g<- step(m8, ~((PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2
            +QROEG+TOVN+TROEG+POVN+CO2+CO+SO2+HCL+H2O), direction="both")

summary(m8_g)
Anova(m8_g, type="II")



par(mfrow=c(2,2))
plot(m8_g)
dev.off()

###
# Estimating the variances between the two labs

kklab <- dioxin %>% filter(LAB == "KK")
var(log(kklab$DIOX))
uslab <- dioxin %>% filter(LAB == "USA")

#w_ <- rep(1/var(log(kklab$DIOX)),57)
w_ <- rep(1,57)
w_[which(dioxin$LAB=="USA")] <- 1/var(log(uslab$DIOX)) 
w_
1/var(log(uslab$DIOX))


w8 <- lm(log(DIOX) ~ PLANT + TIME + LAB + O2COR + NEFFEKT + QRAT + PLANT:QRAT + 
     TIME:NEFFEKT, data=dioxin, weights=w_)

summary(w8)
par(mfrow=c(2,2))
plot(w8)
summary(m8_g)


##########
# Using boxcox transformed data for the same analysis as up above

model2.red_bc <- lm(trans ~ (PLANT+TIME+LAB+O2COR+NEFFEKT), data=dioxin)
m8bc <-step(model2.red_bc, ~(PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2, direction="forward")
m8bc_g <- step(m8bc, ~((PLANT+TIME+LAB+O2COR+NEFFEKT+QRAT)^2
           +QROEG+TOVN+TROEG+POVN+CO2+CO+SO2+HCL+H2O), direction="both")
summary(m8bc_g)
summary(m8bc)
Anova(m8bc, type="II")

plot(m8bc)
