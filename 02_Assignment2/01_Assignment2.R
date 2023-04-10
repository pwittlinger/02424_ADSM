#install.packages("ggfortify")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("Hmisc")
#install.packages("nlme")
#install.packages("sjPlot")
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
AIC(lm.no)

# Now fitting seconder order interactions between the variables
lm.s2 <- lm(clo~(tOut+tInOp+sex)^2, data=df1)
summary(lm.s2)
anova(lm.no, lm.s2)
plot(lm.s2)
AIC(lm.s2)

# Third oder interactions
lm.1 <- lm(clo~(tOut+tInOp+sex)^3, data=df1)
summary(lm.1)


par(mfrow=c(2,2))
plot(lm.1)
logLik(lm.1)
anova(lm.s2, lm.1)
summary(lm.1)
AIC(lm.1)

# Even the fully specified model is quite poor at explaining the variation in the data

lm.2 <- lm(clo~poly(tOut,2)*sex*poly(tInOp,2), data=df1)
summary(lm.2)
AIC(lm.2)
anova(lm.1, lm.2)
logLik(lm.2)
plot(lm.2)
# Backwards selection leads to the same model, meaning it already is the best model to choose
lm.2small <- step(lm.2)
summary(lm.2small)



png("gaussianpoly.png")
par(mfrow=c(2,2))
plot(lm.2)
dev.off()
anova(lm.2,lm.1)




#######
# GAMMA MODEL
# Since even a quite extensive gaussian model is not sufficient we expand our research to other distributions
mean(data$clo)
glm.no <- glm(clo~(tOut+tInOp+sex), family = Gamma, data = df1)
summary(glm.no)
glm.no$coefficients
plot(glm.no)
# McFadden R-squared
1-glm.no$deviance/glm.no$null.deviance
AIC(glm.no)


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
AIC(glm.1)

anova(glm.0, glm.1, test="F")
# Increasing the model order any further does not yield any significant improvement in explanatory power

# Taking higher order polynomials 
glm.2 <- glm(clo~poly(tOut,2)*sex*poly(tInOp,2),family=Gamma(link = "inverse"), data=df1)
summary(glm.2)
AIC(glm.2)
logLik(glm.2)
par(mfrow=c(2,2))
plot(glm.2)

plot(glm.2$coefficients, )
summary(glm.2)
ci_params <- confint(glm.2)

# Plotting model parameters + their corresponding confidence interval
library(sjPlot)
png("model_paramsplot.png")
plot_model(glm.2, vline.color = "red", transform=NULL)
dev.off()

knitr::kable(Anova(glm.2, type="II"), "latex", digits=2)

knitr::kable(summary(glm.2)$coefficients, "latex", digits=2)

1-glm.2$deviance/glm.2$null.deviance
summary(glm.2)
plot(glm.2$residuals)
Anova(glm.2, type="II")

dev.off()

png("gammapoly.png")
par(mfrow=c(2,2))
plot(glm.2)
dev.off()
anova(lm.2,lm.1)

anova(glm.0, glm.2, test="F")

glm.3 <- glm(clo~poly(tOut,3)*sex*poly(tInOp,3),family=Gamma(link = "inverse"), data=df1)
#summary(glm.2)
#step(glm.2)
#logLik(glm.2)
#par(mfrow=c(2,2))
#plot(glm.2)

#anova(glm.3, glm.2, test="F")

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
df2$clo

glm.2subj <- glm(clo~poly(tOut,2)*subjId*poly(tInOp,2),family=Gamma, data=df2)
AIC(glm.2subj)

glm.2subjint <- glm(clo~(poly(tOut,2)+subjId+poly(tInOp,2))^2,family=Gamma, data=df2)

summary(glm.2subj)
summary(glm.2subjint)

step(glm.2subj, direction = "backward")
drop1(glm.2subj)
anova(glm.2subj, test="F")
png("subjIDModel.png")
par(mfrow=c(2,2))
plot(glm.2subj)
dev.off()

glm.0subj <-glm(clo~(tOut)*subjId*(tInOp),family=Gamma, data=df2)
AIC(glm.0subj)
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




###
# Estimating the variances between the two genders

###############
# getting specific Genders

data.frame(model.matrix(glm.no))
summary(glm.no)

y_male <- df1 %>% filter(sex == 'male') #%>% select(logdioxin)
y_male <- y_male$clo
y_female <- df1 %>% filter(sex == "female") #%>% select(logdioxin)
y_female <- y_female$clo


design_matrix <- data.frame(model.matrix(glm.2subj))
X_male <- design_matrix %>% filter(sexmale == 1)
X_male
X_female <- design_matrix %>% filter(sexmale == 0)
X_female



length(colnames(X_male))
#optim function
likelihood.std <- function(params, y_male, X_male, y_female, X_female, theta_male, theta_female){
  len_betas <- length(colnames(X_male))
  betas = params[1:len_betas]
  theta_male <- 1/theta_male
  theta_female <- 1/theta_female
  mean_male=data.matrix(X_male)%*%betas
  neta_male <- 1/mean_male
  mean_female=data.matrix(X_female)%*%betas
  neta_female <- 1/mean_female
  #print(params[len_betas+2])
  llmale <- sum(dgamma(unlist(y_male), shape = neta_male/theta_male, scale = theta_male, log = TRUE))
  llfemale <-sum(dgamma(unlist(y_female), shape = neta_female/theta_female, scale = theta_female, log = TRUE))
  return(-(llmale+llfemale))
}
dnorm(c(1,2,1,1), mean = c(0,0,0,0), sd = 1)
# optimizing it
params <- rep(0.01, length(glm.no$coefficients)+1)
#params <- m8_g$coefficients
params

glm.no$coefficients
likelihood.std(params, y_male, X_male, y_female, X_female)
glm.no$coefficients
summary(glm.no)$dispersion
coefficients <- coef(glm.no)

# extract the dispersion parameter from the model summary
dispersion <- summary(glm.no)$dispersion

# concatenate the coefficients and dispersion parameter into an array
coef_array <- c(coefficients, dispersion)
coef_array
likelihood.std(coef_array, y_male, X_male, y_female, X_female)
lower <- rep(0.001,length(glm.no$coefficients)+1)
lower[length(glm.no$coefficients)+1]<- 0.001
lower[length(glm.no$coefficients)+1]<- 0.01
lower
max_iter = 5000
#result_opt <- optim(params, likelihood.std, y_male=y_male, X_male=X_male, y_female=y_female, X_female = X_female, hessian=TRUE)
result_opt <- optimx(params, likelihood.std, y_male=y_male, X_male=X_male, y_female=y_female, X_female = X_female, hessian=TRUE,  method="L-BFGS-B", lower = lower, control = list(maxit = max_iter))
warnings()
result_opt
null_array <- c()
null_array
iter <-0
params <- rep(0.01, length(glm.no$coefficients))
lower <- rep(0.001,length(glm.no$coefficients))
for (theta_male in 1:80) {
  for (theta_female in 1:80) {
    result_opt <- optimx(params, likelihood.std, y_male=y_male, X_male=X_male, y_female=y_female, X_female = X_female,
                         theta_male = theta_male, theta_female=theta_female,
                         hessian=TRUE,  method="L-BFGS-B", lower = lower)
    results <- c(theta_male, theta_female, result_opt$value)
    null_array <- cbind(null_array, list(results))
    iter<- iter+1
    print(iter)
    print(results)
  }
}
profile_df <- data.frame(do.call(rbind,null_array))

profile_df
colnames(profile_df) <- c('param1', 'param2', 'LogLik')
library(ggplot2)
profile_df

filter(profile_df, LogLik == min(LogLik))
min_vals <- dplyr::select(filter(profile_df, LogLik == min(LogLik)),param1, param2)
min_vals
data.frame(profile_df)
ggplot2::ggplot(profile_df, ggplot2::aes(x = param1, y = param2, z = LogLik)) +
  ggplot2::geom_contour() +
  #ggplot2::geom_point(data = min_vals, ggplot2::aes(x = param1, y = param2), size = 3, color = "red") +
  ggplot2::labs(x = "sigma male", y = "sigma female", z = "Log Likelihood Ratio") +
  ggplot2::theme_bw()

