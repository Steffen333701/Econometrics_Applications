
########## Problem set 2 ###########

# clean space and set wd
ls()
rm(list = ls())
setwd("C:/Users/Steffen_Laptop/Documents/R/Econometrics_applications/Econometrics_Applications")

# Packages
library(tidyverse)
library(psych)
library(skimr)

################################## 1 ################################# 


#read the data
dat.ps2.evs <- read.csv(file = "ps2_EVS.csv")


describe(dat.ps2.evs)



################################## 2 Conitional expectation function #################################

##############  2.1Age  #################################
conditexp.age  <- data.frame(Age = 0, mean.of.satisfaction = 0)

for (i in min(dat.ps2.evs$Age):max(dat.ps2.evs$Age))
{
  conditexp.age[i,1] <- i
  conditexp.age[i,2] <- mean(dat.ps2.evs[dat.ps2.evs$Age == i, "Satisfaction"])}

ggplot(data=conditexp.age[conditexp.age$Age >= min(dat.ps2.evs$Age),], aes(x=Age, y=mean.of.satisfaction)) + geom_line()



#############   2.2Unemployment #################################
conditexp.unemployed  <- data.frame(Unemployed = 0, mean.of.satisfaction = 0)

for (i in min(dat.ps2.evs$Unemployed):max(dat.ps2.evs$Unemployed))
{
  conditexp.unemployed[i+1,1] <- i
  conditexp.unemployed[i+1,2] <- mean(dat.ps2.evs[dat.ps2.evs$Unemployed == i, "Satisfaction"])}

ggplot(data=conditexp.unemployed[conditexp.unemployed$Unemployed >= min(dat.ps2.evs$Unemployed),], aes(x=Unemployed, y=mean.of.satisfaction)) + geom_line()

#perfectly linear because binary variable

#3 estimate linear regression, heteroskedastic se
reg.model.1<-lm(Satisfaction ~ Unemployed, data=dat.ps2.evs)
summary(reg.model.1)

# on average an unemployed person is 0.86 points less happy than an employed person


######## 4. Heteroskedatstik errors ###########
regrr = model.matrix(~., data=dat.ps2.evs %>% select (Unemployed))
coeff.4<- data.frame(matrix(unlist(reg.model.1[1]), nrow=2, byrow=T))

err.4 <- dat.ps2.evs$Satisfaction - coeff.4[2,1] * dat.ps2.evs$Unemployed - coeff.4[1,1]
eps.sqr <- t(err.4)*err.4
#problem
eps.var <- diag(data.frame(eps.sqr))

var.het = solve(t(regrr)%*%regrr) %*% t(regrr) %*% eps.sqr %>% regrr %*% solve(t(regrr)*regrr)



#5
reg.model.2<-lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed*PartnerUnemployed , data=dat.ps2.evs)
summary(reg.model.2)
s.2 <- summary(reg.model.2)


#6 hetroskedasticity consistent se



#7
gradient <- t(c(s.2$coefficients[c(3,2),1],-1))
var.ols <- diag(s.2$coefficients[2:4,2]^2)

variance.delta = sqrt(gradient %*% var.ols %*% t(gradient))

t.stat = (s.2$coefficients[3,1]*s.2$coefficients[2,1]-1)/variance.delta

#8 non parametrics bootstrap ##############


n = 1000
B = 10000
est.coeff = matrix(0, nrow=B, ncol = 8)

for (i in 1:B){
choice = floor(runif(n,1,nrow(dat.ps2.evs)+1))
new.dta <- dat.ps2.evs[choice,]

reg.model.3<-lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed*PartnerUnemployed , data=new.dta)
summary(reg.model.3)
s.3 <- summary(reg.model.3)

est.coeff[i,] <- c(s.3$coefficients[,1], s.3$coefficients[,2])
}

beta.bot = est.coeff[,1:4]
beta.means = colMeans(beta.bot)
beta.diff = beta.bot - beta.means

for (i in 1:B){
v.hat.beta = t(t(beta.diff[i,])) %*% beta.diff[i,] / B
}


########### Part 2 ###################

#1 Main findings of paper #############

##### 2 ####################
dat.ps2.Penn <- read.csv(file = "ps2_Penn.csv")

dat.ps2.merge <- merge(dat.ps2.evs ,dat.ps2.Penn, by=c("country", "year"))
# county and year not in ps2_penn

###### 3 ###################

##############  2.1Age  #################################
conditexp.emppoptatio  <- data.frame(empPopRatio = 0, mean.of.satisfaction = 0)
values = unique(dat.ps2.merge$empPopRatio[order(dat.ps2.merge$empPopRatio)])

for (i in 1:length(values))
{
  conditexp.emppoptatio[i,1] <- i
  conditexp.emppoptatio[i,2] <- mean(dat.ps2.merge[dat.ps2.merge$empPopRatio == values[i], "Satisfaction"])}

ggplot(data=conditexp.emppoptatio, aes(x=empPopRatio, y=mean.of.satisfaction)) + geom_line()



#############   2.2Unemployment #################################
conditexp.unemployed  <- data.frame(Unemployed = 0, mean.of.satisfaction = 0)

for (i in min(dat.ps2.evs$Unemployed):max(dat.ps2.evs$Unemployed))
{
  conditexp.unemployed[i+1,1] <- i
  conditexp.unemployed[i+1,2] <- mean(dat.ps2.evs[dat.ps2.evs$Unemployed == i, "Satisfaction"])}

ggplot(data=conditexp.unemployed[conditexp.unemployed$Unemployed >= min(dat.ps2.evs$Unemployed),], aes(x=Unemployed, y=mean.of.satisfaction)) + geom_line()


