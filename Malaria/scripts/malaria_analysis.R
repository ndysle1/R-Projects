# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Data Analysis #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# This script takes performs exploratory analysis 


# Packages/Functions ----
source('Malaria/functions/malaria_functions.R')

##More Useful packages for analysis##
library(caTools)
library(e1071)
library(glmnet)
library(ISLR)
library(leaps)
library(caret)
library(MASS)
library(Hmisc)
library(broom)
library(aod)
library(stargazer)
library(huxtable)

##More Exploratory Analysis##

names(dfadults)
names(dfkids)

str(dfkids)
str(dfadults)

summary(dfadults$SHPROV)
summary(dfkids$SPROV)

apply(is.na(dfkids),2,sum)
apply(is.na(dfadults),2,sum)

l <- ggplot(dfkids, aes(V025,fill=H22))
l <- l + geom_histogram(stat="count")
print(l)

l <- ggplot(dfkids, aes(SPROV,fill=H22))
l <- l + geom_histogram(stat="count")
print(l)

##Correlation Matrix##
my_data <- dfkids[,c(13,14,15,16,17,18,19)]

res <- cor(my_data)
round(res, 2)

my_data <- dfadults[,c(15,16,17,18,19,20,21)]

res <- cor(my_data)
round(res, 2)

##Correlation Graph##
M<-cor(dfkids[,c("Temp", "Temp2", "Precip", "Precip2","DeforestLag3", "DeforestLag2", "DeforestLag1")])
corrplot(M, method = "circle")

M<-cor(dfadults[,c("Temp", "Temp2", "Precip", "Precip2","DeforestLag3", "DeforestLag2", "DeforestLag1")])
corrplot(M, method = "circle")

##Graphing Dependent Variables##
ggplot(dfkids, aes(V025,fill=H22)) + geom_histogram(stat="count")

ggplot(dfadults, aes(HV025,fill=HML35)) + geom_histogram(stat="count")

ggplot(dfadults, aes(HV025,fill=HML32))+ geom_histogram(stat="count")

ggplot(dfkids, aes(SPROV,fill=H22)) + geom_histogram(stat="count") + labs(x="Provinces", y="Individuals", title = "Fever in Last Two Weeks")+ theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name="Fever",labels=c("No", "Yes"))+ scale_x_discrete(breaks=c("1","2","3","4","5","7"),labels=c("ANTA", "FIAN", "TOAM", "MAHA","TOLI","ANTS"))

ggplot(dfadults, aes(SHPROV,fill=HML32))+ geom_histogram(stat="count") + labs(x="Provinces", y="Individuals", title = "Blood Smear Results")+ theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name="Result",labels=c("Negative", "Positive"))+ scale_x_discrete(breaks=c("1","2","3","4","5","7"),labels=c("ANTA", "FIAN", "TOAM", "MAHA","TOLI","ANTS"))

ggplot(dfadults, aes(SHPROV,fill=HML35))+ geom_histogram(stat="count") + labs(x="Provinces", y="Individuals", title = "Rapid Test Results")+ theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name="Result",labels=c("Negative", "Positive"))+ scale_x_discrete(breaks=c("1","2","3","4","5","7"), labels=c("ANTA", "FIAN", "TOAM", "MAHA","TOLI","ANTS"))

##Checking the amount of individuals that had malaria vs did not have malaria per province and testing type##
tapply(as.numeric(dfadults$HML32)-1,dfadults$SHPROV,mean)
tapply(as.numeric(dfadults$HML35)-1,dfadults$SHPROV,mean)
tapply(as.numeric(dfkids$H22)-1,dfkids$SPROV,mean)

fever <- factor(dfkids$H22, c(0,1))
Prov <- factor(dfkids$SPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

fever <- factor(dfadults$HML32, c(0,1))
Prov <- factor(dfadults$SHPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

fever <- factor(dfadults$HML35, c(0,1))
Prov <- factor(dfadults$SHPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

