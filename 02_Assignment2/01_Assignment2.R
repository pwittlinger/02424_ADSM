#install.packages("ggfortify")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("Hmisc")
#install.packages("nlme")
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
data<-read.csv("../02_Assignment2/clothing.csv",header=T)
names(data)
# first step -> descriptive analytics of the dependent variable
describe(data$clo)
sd(data$clo)
mean(data$clo)
# the data is quite spread out

plot(data$clo, main = "clo", ylab="clo", xlab="Observation")
hist(data$clo)
hist(log(data$clo))
#hist(data$clo, main = "clo", ylab="clo", density = 100)
GGally::ggpairs(data)

# Transforming the variables into factors
data <- mutate_if(data, is.character, as.factor)
data$day <- as.factor(data$day)
data$subjId <- as.factor(data$subjId)
data$X <- NULL

df <- data  %>% dplyr::select(where(is.numeric))
df$day <- data$day
df$sex <- data$sex

pairs(df)
GGally::ggpairs(df)


##############
## Exercise 1
# Fit a GLM on the data only using clo,tInOP, tOut
####

df1 <- dplyr::select(data,c("clo", "tOut", "tInOp", "sex"))

lm.1 <- lm(clo~(tOut+tInOp+sex)^3, data=df1)
summary(lm.1)
plot(lm.1)
logLik(lm.1)


lm.2 <- lm(clo~poly(tOut,2)*sex*poly(tInOp,2), data=df1)
summary(lm.2)

par(mfrow=c(2,2))
plot(lm.2)
dev.off()

glm.1 <- glm(clo~(tOut+tInOp+sex)^3, family = Gamma, data = df1)
summary(glm.1)
logLik(glm.1)

glm.2 <- glm(clo~poly(tOut,2)*sex*poly(tInOp,2),family=Gamma, data=df1)
summary(glm.2)
logLik(glm.2)
par(mfrow=c(2,2))
plot(glm.2)


##############
#






# Looking at the raw dependent variable
## we can see that there are quite some outliers
#png(filename = "dioxin.png", width = 9600, height = 4800, units="px", res=600)
#par(mfrow=c(1,2))
#plot(dioxin$DIOX, main = "Dioxin in Original Domain", ylab="Dioxin", xlab="Observation")
#hist(dioxin$DIOX, main="Distribution of Dioxin levels (original domain)")
#dev.off()




