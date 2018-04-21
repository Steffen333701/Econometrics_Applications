
# Problem set 1


# clean space and set wd
ls()
rm(list = ls())
setwd("C:/Users/Steffen_Laptop/Documents/R/Econometrics_applications/Econometrics_Applications")

# Packages
library(tidyverse)
library(psych)
library(skimr)

########################################################################


# load the data 
datps1 <- read.csv(file = "ps1_nls.csv")

# 1
summary(datps1)
sapply(datps1, sd, na.rm=TRUE)

describe(datps1)
skim(datps1)
datps1 %>% summarise_all (.funs = funs( mean = mean(.), sd = sd(.), min = min(.), max = max(.))) %>%
gather() %>%
  serarate (key, into=c("variable", "stat"), sep = "_") %>%
              dcast (formula = variable ~ stat)

            
# iq of 50 is kind of low?

#2

hist(datps1$logWage)
hist(datps1$educ)
hist(datps1$age)
hist(datps1$iq)

ggplot(data=datps1, aes(datps1$logWage)) + geom_histogram(binwidth = 0.03)
ggplot(data=datps1, aes(datps1$educ)) + geom_histogram(binwidth = 1)
ggplot(data=datps1, aes(datps1$age)) + geom_histogram(binwidth = 1)
ggplot(data=datps1, aes(datps1$iq)) + geom_histogram(binwidth = 1)


#3
conditexp  <- data.frame(educ = Na, mean.of.logwage = Na)

for (i in min(datps1$educ):max(datps1$educ))
     {
        conditexp[i,1] <- i
        conditexp[i,2] <- mean(datps1[datps1$educ == i, "logWage"])}

ggplot(data=conditexp[conditexp$educ >= min(datps1$educ),], aes(x=educ, y=mean.of.logwage)) + geom_line()

#4
reg.model<-lm(logWage ~ educ, data=datps1)
summary(reg.model)


ggplot(data=conditexp[conditexp$educ >= min(datps1$educ),], aes(x=educ, y=mean.of.logwage)) + 
  geom_line(color = "blue", size= 2) +
  geom_abline(intercept = reg.model$coefficients[1], slope = reg.model$coefficients[2], color="red", size= 2)

## 5 ====================

#one more year of educations increases the logwage by 6%
#two more years of education increases logwage by 12%

## 6 ====================
datps1$exper <- datps1$age - datps1$educ - 6

## 7 ====================
reg.model.2 <- lm(logWage ~ educ + exper + I(exper^2), data = datps1)
summary(reg.model.2)

## 8 ====================
 
coeff.8<- data.frame(matrix(unlist(reg.model.2[1]), nrow=4, byrow=T))
ern.8a <- coeff.8[1,1] + coeff.8[2,1] * 9  + coeff.8[3,1] * (40-9-6) + coeff.8[4,1] * (40-9-6)^2
ern.8b <- coeff.8[1,1] + coeff.8[2,1] * 13  + coeff.8[3,1] * (40-13-6) + coeff.8[4,1] * (40-13-6)^2
ern.8.diff = ern.8a - ern.8b


## 9 ====================

dat.ps1.refactored <- data.frame(datps1$logWage ,datps1$educ, datps1$educ + datps1$exper, datps1$educ*46 + datps1$exper^2)
colnames(dat.ps1.refactored) <- c("A", "B", "C", "D")

reg.model.5 <- lm(A ~ B+C+D, data=dat.ps1.refactored)
summary(reg.model.5)

coeff.9 = data.frame(matrix(unlist(reg.model.5[1]), nrow=4, byrow=T))
ern.9.diff = coeff.9[2,1] * (9-13)

## 10 ===================
reg.model.3 <- lm(logWage ~ educ + exper + I(exper^2) + age, data = datps1)
summary(reg.model.3)

# Here one can see perfect multicollinearity


## 11 ===================
# Omitted variable bias
# expect coefficient on education to become smaller
# education has a positive influence on earnings and i expect education and iq to be positively correlated


## 12 ===================
reg.model.4 <- lm(logWage ~ educ + iq, data = datps1)
summary(reg.model.4)

# i expect and 10 point iq increase to increase earnings by 4%




