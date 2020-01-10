library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

setwd("G:\\R Language\\session 6\\market campaign linear regression")
data<- read.csv("Marketingdata.csv")
str(data)
summary(data) 
## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))
#there is no missing value in the table
str(data)
data$MarketID <- as.factor(data$MarketID)
data$Promotion <- as.factor(data$Promotion)
data$week <- as.factor(data$week)
str(data)
boxplot(data$SalesInThousands)
## Checking for outliers
quantile(data$SalesInThousands, c(0,0.05,0.1,0.25,0.5,0.75,0.85,0.90,0.95,0.99,0.995,1))
data<-data[data$SalesInThousands<82, ]
boxplot(data$SalesInThousands)
nrow(data)
fit<-lm(SalesInThousands~MarketID +	MarketSize +	LocationID +	AgeOfStore + Promotion + week , data=data)
summary(fit)
fit<-lm(SalesInThousands~MarketID+ Promotion , data=data)
summary(fit)
fit<- lm(SalesInThousands ~ I(MarketID==2) +I(MarketID==3) +I(MarketID==4) +I(MarketID==5) 
         +I(MarketID==7) +I(MarketID==8) +I(MarketID==9) +I(MarketID==10)+ Promotion , data=data)
summary(fit)
## Under normal conditions you should negate the promotions since it has a negetive sign.
## However, we do not know what type of promotion it is. Might be a negetive promotion (cigarette promotion)
## If promotion is there in the model the R square increases and MAPE decreases
fit<- lm(SalesInThousands ~ I(MarketID==2) +I(MarketID==3) +I(MarketID==4) +I(MarketID==5) 
         +I(MarketID==7) +I(MarketID==8) +I(MarketID==9) +I(MarketID==10) , data=data)
summary(fit)
###model validation check###
##R-squared value is 0.6927~0.7 that is sign of strong regression model
## MAPE
data$pred <- fitted(fit)#it will give predicted salesinthousands

#Calculating MAPE
attach(data) 
#by using attach()we can use pred globally
(sum((abs(SalesInThousands-pred))/SalesInThousands))/nrow(data)

##MAPE value is 0.1141003 , it is very less than so this is better model
#Check Vif, vif>2 means presence of multicollinearity
vif(fit)
# Vif value is less than 2 ,so NULL hypothesis:-there is no multicollinearity present in between predictors

####Checking of assumption#####

###1..multicollinearity###
vif(fit)
##No multicollnearity is present in between predictors

###2..Autocorrelation
# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value..using durbin-watson Test autocorrelation Test can be done

dwt(fit)
## dwt value is 1.659 that is close to 2 , so Ho:-residuals from linear regression are uncorrelated

##3...Homoscedasticity Test###
# Breusch-Pagan test
# Null hypothesis -> error is homogenious (p value should be more than 0.05)
bptest(fit)
##H1: error is not homogenios satisfies Ho violates

##4..Normality Test...
##Ho:- Error is normally distributed or data is normally distributed
## Normality testing Null hypothesis is data is normal.(p-value should be greater than 0.05)

resids <- fit$residuals
ad.test(resids) #get Anderson-Darling test for normality
###p_value-0.6188>0.05 so the Null Hypothesis HO:-data is normal



