library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("GGally")
#library(GGally)
library(MASS)
library(car)
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
hist(y2)

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


model <- lm(DIOX ~ (PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK)^2, data=dioxin)
alias(model)
summary(model)
Anova(model, type="II")
(fit.back <- step(model, direction="back"))

step(lm(DIOX~1, data=dioxin), ~PLANT*TIME*LAB*OXYGEN*LOAD*PRSEK, direction="forward")


step(lm(DIOX~1, data=dioxin), ~PLANT*TIME*LAB*OXYGEN*LOAD*PRSEK, direction="both")
