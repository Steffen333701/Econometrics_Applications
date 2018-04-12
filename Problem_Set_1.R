
# Problem set 1


# clean space and set wd
ls()
rm(list = ls())
setwd("C:/Users/Steffen_Laptop/Documents/R/Econometrics_applications/Econometrics_Applications")

# Packages
library(tidyverse)

########################################################################


# load the data 
datps1 <- read.csv(file = "ps1_nls.csv")

# 1
summary(datps1)
sapply(datps1, sd, na.rm=TRUE)

#2

hist(datps1$logWage)
hist(datps1$educ)
hist(datps1$age)
hist(datps1$iq)

ggplot(data=datps1, aes(datps1$logWage)) + geom_histogram(binwidth = 0.03)
ggplot(data=datps1, aes(datps1$educ)) + geom_histogram(binwidth = 1)
ggplot(data=datps1, aes(datps1$age)) + geom_histogram(binwidth = 1)
ggplot(data=datps1, aes(datps1$iq)) + geom_histogram(binwidth = 1)




