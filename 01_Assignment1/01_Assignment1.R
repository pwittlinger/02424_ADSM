library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
### Assignment1
# Reading in the data
dioxin<-read.csv("../01_Assignment1/dioxin.csv",header=T)
attach(dioxin)
names(dioxin)

# Transforming the variables into factors
dioxin <- mutate_if(dioxin, is.character, as.factor)
