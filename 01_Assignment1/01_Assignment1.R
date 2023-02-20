library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("GGally")
#library(GGally)
### Assignment1
# Reading in the data
dioxin<-read.csv("../01_Assignment1/dioxin.csv",header=T)
names(dioxin)

# Transforming the variables into factors
dioxin <- mutate_if(dioxin, is.character, as.factor)


df <- dioxin  %>% dplyr::select(where(is.numeric))
pairs(df)


p <- ggplot(dioxin, aes(x=PLANT, y=DIOX, fill=LAB)) + geom_boxplot(outlier.size = 2, notch=FALSE, outlier.shape = 16, outlier.colour = "black")
p

