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

png(filename="pairplot.png")
dplyr::select(df,c("clo", "tOut", "tInOp", "sex")) %>% GGally::ggpairs()
dev.off()


##############
## Exercise 1
# Fit a GLM on the data only using clo,tInOP, tOut
####

df1 <- dplyr::select(data,c("clo", "tOut", "tInOp", "sex"))

# first we fit the simplest model without any interactions
lm.no <- lm(clo~tOut+tInOp+sex, data=df1)
summary(lm.no)
par(mfrow=c(2,2))
plot(lm.no)

lm.s2 <- lm(clo~(tOut+tInOp+sex)^2, data=df1)
summary(lm.s2)
anova(lm.no, lm.s2)
plot(lm.s2)


lm.1 <- lm(clo~(tOut+tInOp+sex)^3, data=df1)
summary(lm.1)


par(mfrow=c(2,2))
plot(lm.1)
logLik(lm.1)
anova(lm.s2, lm.1)
summary(lm.1)

# Even the fully specified model is quite poor at explaining the variation in the data

lm.2 <- lm(clo~poly(tOut,2)*sex*poly(tInOp,2), data=df1)
summary(lm.2)
AIC(lm.2)
anova(lm.1, lm.2)
logLik(lm.2)

# Backwards selection leads to the same model, meaning it already is the best model to choose
#lm.2small <- step(lm.2)
#summary(lm.2small)

par(mfrow=c(2,2))
plot(lm.2)
dev.off()

####
#

glm.no <- glm(clo~(tOut+tInOp+sex), family = Gamma, data = df1)
summary(glm.no)
plot(glm.no)

glm.0 <- glm(clo~(tOut+tInOp+sex)^2, family = Gamma(link = "inverse"), data = df1)
summary(glm.0)
#glm(clo~(tOut+tInOp+sex)^2, family = Gamma, data = df1) %>% summary()
plot(glm.0)
anova(glm.no, glm.0, test="F")
anova(glm.0, test="F")
AIC(glm.0)


glm.1 <- glm(clo~(tOut+tInOp+sex)^3, family = Gamma, data = df1)
summary(glm.1)
plot(glm.1)
logLik(glm.1)

anova(glm.0, glm.1, test="F")
# Increasing the model order any further does not yield any significant improvement in explanatory power

glm.2 <- glm(clo~poly(tOut,2)*sex*poly(tInOp,2),family=Gamma, data=df1)
summary(glm.2)
logLik(glm.2)
par(mfrow=c(2,2))
plot(glm.2)

anova(glm.0, glm.2, test="F")


##############
#

#glm.0$residuals %>% dplyr::select

#df$sex %>% dplyr::select(0)

resdat <- cbind(glm.0$residuals, df$sex)
colnames(resdat) <- c("residuals", "sex")
resdat <- data.frame(resdat)
resdat$sex <- as.factor(resdat$sex)
resdat$sex <- df$sex

p <- ggplot(resdat, aes(x=sex, y=residuals)) + geom_boxplot(outlier.size = 2, notch=FALSE, outlier.shape = 16, outlier.colour = "black")
p

var_m <- resdat %>% dplyr::filter(sex=="male") %>% dplyr::select("residuals")# %>% var

var_f <- resdat %>% dplyr::filter(sex=="female") %>% dplyr::select("residuals")# %>% var


var.test(unlist(var_m), unlist(var_f))

######
# Fit the model now with subjectId instead of 

df2 <- dplyr::select(data,c("clo", "tOut", "tInOp", "subjId"))

glm.2subj <- glm(clo~poly(tOut,2)*subjId*poly(tInOp,2),family=Gamma, data=df2)

summary(glm.2subj)
anova(glm.2subj, test="F")
plot(glm.2subj)

glm.0subj <-glm(clo~(tOut)*subjId*(tInOp),family=Gamma, data=df2)
summary(glm.0subj)
plot(glm.0subj)

anova(glm.0subj,glm.2subj, test="F")


glm.2subj$residuals

df_res <- df2
df_res$residuals <- glm.2subj$residuals
df_res$day <- data$day
df_res$time <- data$time


df_res %>% dplyr::group_by(subjId,day) %>% dplyr::arrange(subjId, day, time)
dev.off()


grouped_df <- df_res %>% dplyr::group_by(subjId,day) %>% summarise(value_list = list(residuals))

grouped_df$value_list[2]

#install.packages("purrr")

library(purrr)

length(grouped_df$value_list)
sapply(grouped_df$value_list, length)

filtered_df <- grouped_df %>% filter(length(value_list)>2)

corr_df <- filtered_df %>% 
  mutate(corr = map2_dbl(array(unlist(value_list))[-1], (unlist(value_list))[-length(unlist(value_list))], ~ cor(.x, .y)))

#corr_df <- filtered_df %>% 
#  mutate(corr = map2_dbl(slice(unlist(value_list), -length(value_list)), slice(unlist(value_list),-1)), ~ cor(.x, .y))

filtered_df %>% sapply(cor(array(unlist(value_list))[-1], (unlist(value_list))[-length(unlist(value_list))]))



#vl <- filtered_df$value_list[1]

cor(unlist(vl)[-1],unlist(vl)[-length(unlist(vl))])
array(unlist(vl))[-1]


df_res$residuals[0:6]

acf(df_res$residuals[0:6],6)

# Looking at the raw dependent variable
## we can see that there are quite some outliers
#png(filename = "dioxin.png", width = 9600, height = 4800, units="px", res=600)
#par(mfrow=c(1,2))
#plot(dioxin$DIOX, main = "Dioxin in Original Domain", ylab="Dioxin", xlab="Observation")
#hist(dioxin$DIOX, main="Distribution of Dioxin levels (original domain)")
#dev.off()


df3 <- df2
df3$sex <- df1$sex


model <- glm(clo~poly(tOut,2)*subjId*poly(tInOp,2),
             family=Gamma(link="log"), data=df3, weights = rep(1,length(df3$sex)), dispersion=c(NA, NA))

# estimate the dispersion parameters via maximum likelihood
model <- glm(y ~ x + class, data = df, family = Gamma(link = "log"), 
             weights = 1/model$fitted.values^2, start = coef(model))

# print the model summary
summary(model)



rep(1,length(df3$sex))

