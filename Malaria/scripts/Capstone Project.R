##Loading useful packages##
library(dplyr)
library(haven)
library(readxl)
library(tidyverse)
library(reshape)
library(reshape2)
library("ggplot2")
library("ggthemes")
library(corrplot)
library(car)

##Set Working Directory##
setwd("C:/Users/User/Desktop/Software Cheats/R/Projects/Malaria")

##Importing Original Datasets##
#df2011pr <- read_sas("mis2011fpr.SAS7BDAT", NULL) #40581 obs. of 283 variables#
#df2013pr <- read_sas("mis2013fpr.SAS7BDAT", NULL) #38776 obs. of 341 variables#
#df2016pr <- read_sas("mis2016fpr.SAS7BDAT", NULL) #49141 obs. of 352 variables#
#df2011f <- read_sas("mis2011f.SAS7BDAT", NULL) #6248 obs. of 941 variables#
#df2013f <- read_sas("mis2013f.SAS7BDAT", NULL) #5477 obs. of 998 variables#
#df2016f <- read_sas("mis2016f.SAS7BDAT", NULL) #6978 obs. of 997 variables#

##The 2016 survey for adults did not include provinces, but rather regions, so provincial variables had to be created that matched the other survey years##
#df2016pr$SHPROV[df2016pr$HV024<19]<-1
#df2016pr$SHPROV[df2016pr$HV024>=20 & df2016pr$HV024<29]<-2
#df2016pr$SHPROV[df2016pr$HV024>=30 & df2016pr$HV024<39]<-3
#df2016pr$SHPROV[df2016pr$HV024>=40 & df2016pr$HV024<49]<-4
#df2016pr$SHPROV[df2016pr$HV024>=50 & df2016pr$HV024<59]<-5
#df2016pr$SHPROV[df2016pr$HV024>=70 & df2016pr$HV024<79]<-7

##Selecting only the needed variables so personal identifiers are no longer shown##
#df2011pr = df2011pr %>%
#  select(HV006, HV007, HV024, HV025, HV201, HV205, HV213, HV214, HV215, HV228, HV253, SHPROV, HML32, HML35)
#df2013pr = df2013pr %>%
#  select(HV006, HV007, SHREGION1, HV025, HV201, HV205, HV213, HV214, HV215, HV228, HV253, SHPROV, HML32, HML35)
#df2016pr = df2016pr %>%
#  select(HV006, HV007, HV024, HV025, HV201, HV205, HV213, HV214, HV215, HV228, HV253, SHPROV, HML32, HML35)

##The 2016 survey for children did not include provinces, but rather regions, so provincial variables had to be created that matched the other survey years##
#df2016f$SPROV[df2016f$V024<19]<-1
#df2016f$SPROV[df2016f$V024>=20 & df2016f$V024<29]<-2
#df2016f$SPROV[df2016f$V024>=30 & df2016f$V024<39]<-3
#df2016f$SPROV[df2016f$V024>=40 & df2016f$V024<49]<-4
#df2016f$SPROV[df2016f$V024>=50 & df2016f$V024<59]<-5
#df2016f$SPROV[df2016f$V024>=70 & df2016f$V024<79]<-7

##Selecting only the needed variables so personal identifiers are no longer shown##
#df2011f = df2011f %>%
#  select(V006, V007, V024, V025, V113, V116, V127, V128, V129, V460, H22, SPROV)
#df2013f = df2013f %>%
#  select(V006, V007, SREGION, V025, V113, V116, V127, V128, V129, V460, H22, SPROV)
#df2016f = df2016f %>%
#  select(V006, V007, V024, V025, V113, V116, V127, V128, V129, V460, H22, SPROV)

##Writing all datasets to CSV so they can be pulled in for future analysis##
#write.csv(df2011f, "C:/Users/User/Desktop/Software Cheats/R/Projects/Malaria/df2011f.csv", row.names = FALSE)
#write.csv(df2011pr, "C:/Users/User/Desktop/Software Cheats/R/Projects/Malaria/df2011pr.csv", row.names = FALSE)
#write.csv(df2013f, "C:/Users/User/Desktop/Software Cheats/R/Projects/Malaria/df2013f.csv", row.names = FALSE)
#write.csv(df2013pr, "C:/Users/User/Desktop/Software Cheats/R/Projects/Malaria/df2013pr.csv", row.names = FALSE)
#write.csv(df2016f, "C:/Users/User/Desktop/Software Cheats/R/Projects/Malaria/df2016f.csv", row.names = FALSE)
#write.csv(df2016pr, "C:/Users/User/Desktop/Software Cheats/R/Projects/Malaria/df2016pr.csv", row.names = FALSE)


##Pulling in Datasets with only the Needed Variables w/out Individual Identifiers##
df2011pr <- read_csv("df2011pr.csv", col_names = TRUE)
df2013pr <- read_csv("df2013pr.csv", col_names = TRUE)
df2016pr <- read_csv("df2016pr.csv", col_names = TRUE)
df2011f <- read_csv("df2011f.csv", col_names = TRUE)
df2013f <- read_csv("df2013f.csv", col_names = TRUE)
df2016f <- read_csv("df2016f.csv", col_names = TRUE)

##Importing other relevant data sets for climate and deforestation##
climate_data <- read_excel("Climate Data.xlsx", sheet = 1)
deforest_data <- read_excel("Deforestation Data.xlsx", sheet = 2)

##Adding Climate Data to the adults data sets##
df2011pr <- merge(x = df2011pr, y = climate_data, by.x=c("HV006", "HV007", "SHPROV"), by.y=c("Month", "Year", "Province"))
df2013pr <- merge(x = df2013pr, y = climate_data, by.x=c("HV006", "HV007", "SHPROV"), by.y=c("Month", "Year", "Province"))
df2016pr <- merge(x = df2016pr, y = climate_data, by.x=c("HV006", "HV007", "SHPROV"), by.y=c("Month", "Year", "Province"))

##Choosing the relevant variables from the deforestation dataset##
deforest_data <- deforest_data %>%
  select(Province, Region, PercLoss2008, PercLoss2009, PercLoss2010, PercLoss2011, PercLoss2012, PercLoss2013, PercLoss2014, PercLoss2015, PercLoss2016)

##Adding Deforestation Data to the adults data sets##
df2011pr <- merge(x = df2011pr, y = deforest_data, by.x=c("SHPROV", "HV024"), by.y=c("Province", "Region"))
df2011pr <- subset(df2011pr, select = -c(PercLoss2011, PercLoss2012, PercLoss2013, PercLoss2014, PercLoss2015, PercLoss2016))
colnames(df2011pr)[colnames(df2011pr)=="PercLoss2008"] <- "DeforestLag3"
colnames(df2011pr)[colnames(df2011pr)=="PercLoss2009"] <- "DeforestLag2"
colnames(df2011pr)[colnames(df2011pr)=="PercLoss2010"] <- "DeforestLag1"

df2013pr <- merge(x = df2013pr, y = deforest_data, by.x=c("SHPROV", "SHREGION1"), by.y=c("Province", "Region"))
df2013pr <- subset(df2013pr, select = -c(PercLoss2008, PercLoss2009, PercLoss2013, PercLoss2014, PercLoss2015, PercLoss2016))
colnames(df2013pr)[colnames(df2013pr)=="PercLoss2010"] <- "DeforestLag3"
colnames(df2013pr)[colnames(df2013pr)=="PercLoss2011"] <- "DeforestLag2"
colnames(df2013pr)[colnames(df2013pr)=="PercLoss2012"] <- "DeforestLag1"

df2016pr <- merge(x = df2016pr, y = deforest_data, by.x=c("SHPROV", "HV024"), by.y=c("Province", "Region"))
df2016pr <- subset(df2016pr, select = -c(PercLoss2008, PercLoss2009, PercLoss2010, PercLoss2011, PercLoss2012, PercLoss2016))
colnames(df2016pr)[colnames(df2016pr)=="PercLoss2013"] <- "DeforestLag3"
colnames(df2016pr)[colnames(df2016pr)=="PercLoss2014"] <- "DeforestLag2"
colnames(df2016pr)[colnames(df2016pr)=="PercLoss2015"] <- "DeforestLag1"

##AT THIS STEP ALL THREE ADULT DATASETS FOR THE YEARS OF THE STUDY ARE COMPLETE##


##Adding Climate Data to Children Datasets##
df2011f <- merge(x = df2011f, y = climate_data, by.x=c("V006", "V007", "SPROV"), by.y=c("Month", "Year", "Province"))
df2013f <- merge(x = df2013f, y = climate_data, by.x=c("V006", "V007", "SPROV"), by.y=c("Month", "Year", "Province"))
df2016f <- merge(x = df2016f, y = climate_data, by.x=c("V006", "V007", "SPROV"), by.y=c("Month", "Year", "Province"))

##Adding Deforestation Data to the Children Datasets##
df2011f <- merge(x = df2011f, y = deforest_data, by.x=c("SPROV", "V024"), by.y=c("Province", "Region"))
df2011f <- subset(df2011f, select = -c(PercLoss2011, PercLoss2012, PercLoss2013, PercLoss2014, PercLoss2015, PercLoss2016))
colnames(df2011f)[colnames(df2011f)=="PercLoss2008"] <- "DeforestLag3"
colnames(df2011f)[colnames(df2011f)=="PercLoss2009"] <- "DeforestLag2"
colnames(df2011f)[colnames(df2011f)=="PercLoss2010"] <- "DeforestLag1"

df2013f <- merge(x = df2013f, y = deforest_data, by.x=c("SPROV", "SREGION"), by.y=c("Province", "Region"))
df2013f <- subset(df2013f, select = -c(PercLoss2008, PercLoss2009, PercLoss2013, PercLoss2014, PercLoss2015, PercLoss2016))
colnames(df2013f)[colnames(df2013f)=="PercLoss2010"] <- "DeforestLag3"
colnames(df2013f)[colnames(df2013f)=="PercLoss2011"] <- "DeforestLag2"
colnames(df2013f)[colnames(df2013f)=="PercLoss2012"] <- "DeforestLag1"

df2016f <- merge(x = df2016f, y = deforest_data, by.x=c("SPROV", "V024"), by.y=c("Province", "Region"))
df2016f <- subset(df2016f, select = -c(PercLoss2008, PercLoss2009, PercLoss2010, PercLoss2011, PercLoss2012, PercLoss2016))
colnames(df2016f)[colnames(df2016f)=="PercLoss2013"] <- "DeforestLag3"
colnames(df2016f)[colnames(df2016f)=="PercLoss2014"] <- "DeforestLag2"
colnames(df2016f)[colnames(df2016f)=="PercLoss2015"] <- "DeforestLag1"

##Rename Region Variables in 2013 to match 2011 and 2016##

colnames(df2013pr)[2] <- "HV024"
colnames(df2013f)[2] <- "V024"

##Combining all 3 years of datasets for both kids and adults datasets##

dfadults <- rbind(df2011pr, df2013pr, df2016pr)
dfkids <- rbind(df2011f, df2013f, df2016f)

##Amending variables to exclude inconclusive results and NA values##
dfkids <- subset(dfkids, H22 != 8) 
dfkids <- na.omit(dfkids)
dfadults <- subset(dfadults, HML35 != 6 & HML32 != 7 & HV253 != 8) 
dfadults <- subset(dfadults, HML32 != 6)

dfadults <- na.omit(dfadults)

str(dfkids)
str(dfadults)

##Creating new precipitation & temperature values due to prior research findings##
dfkids$Precip2 <- (dfkids$Precip)^2
dfadults$Precip2 <- (dfadults$Precip)^2

dfkids$Temp2 <- (dfkids$Temp)^2
dfadults$Temp2 <- (dfadults$Temp)^2

##Changing the provinces to have names and be dummy variables##
dfadults$Tana <- ifelse(dfadults$SHPROV==1,1,0)
dfadults$Fianar <- ifelse(dfadults$SHPROV==2,1,0)
dfadults$Tomas <- ifelse(dfadults$SHPROV==3,1,0)
dfadults$Mahaj <- ifelse(dfadults$SHPROV==4,1,0)
dfadults$Toli <- ifelse(dfadults$SHPROV==5,1,0)
dfadults$Diego <- ifelse(dfadults$SHPROV==7,1,0)

dfkids$Tana <- ifelse(dfkids$SPROV==1,1,0)
dfkids$Fianar <- ifelse(dfkids$SPROV==2,1,0)
dfkids$Tomas <- ifelse(dfkids$SPROV==3,1,0)
dfkids$Mahaj <- ifelse(dfkids$SPROV==4,1,0)
dfkids$Toli <- ifelse(dfkids$SPROV==5,1,0)
dfkids$Diego <- ifelse(dfkids$SPROV==7,1,0)

##Adjusting variables to simplify the outcomes of the survey##
##For example - instead of listing all types of water sources, the variable is changed to protected vs unprotected water sources##
dfkids$V113 <- ifelse(dfkids$V113 %in% c(32,40,42,43,51),0,1)
dfkids$V116 <- ifelse(dfkids$V116 %in% c(30,31,41,42,43,96,97),0,1)
dfkids$V460 <- ifelse(dfkids$V460 %in% c(0,3), 0,1)
dfkids$V127 <- ifelse(dfkids$V127 %in% c(10,11,12,21,22,23),0,1)
dfkids$V128 <- ifelse(dfkids$V128 %in% c(10,11,12,13,20,21,22,23,24,25),0,1)
dfkids$V129 <- ifelse(dfkids$V129 %in% c(10,11,12,13,20,21,22,23,24),0,1)
dfkids$V025 <- ifelse(dfkids$V025 %in% c(2),0,1)

dfadults$HV201 <- ifelse(dfadults$HV201 %in% c(32,40,42,43,51),0,1)
dfadults$HV205 <- ifelse(dfadults$HV205 %in% c(30,31,41,42,43,96),0,1)
dfadults$HV228 <- ifelse(dfadults$HV228 %in% c(0,3), 0,1)
dfadults$HV213 <- ifelse(dfadults$HV213 %in% c(10,11,12,21,22,23),0,1)
dfadults$HV214 <- ifelse(dfadults$HV214 %in% c(10,11,12,21,22,23),0,1)
dfadults$HV215 <- ifelse(dfadults$HV215 %in% c(10,11,12,13,20,21,22,23,24),0,1)
dfadults$HV025 <- ifelse(dfadults$HV025 %in% c(2),0,1)

str(dfkids)
str(dfadults)

##Changing certain variables to factors##
dfkids <- mutate_at(dfkids, c("SPROV","V024", "V006", "V007", "V025", "V113", "V116", "V127", "V128", "V129", "V460", "H22"), as.factor)
dfadults <- mutate_at(dfadults, c("SHPROV","HV024", "HV006", "HV007", "HV025", "HV201", "HV205", "HV213", "HV214", "HV215", "HV228", "HV253", "HML32","HML35"), as.factor)

summary(dfkids$V113)
summary(dfadults$HV025)

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

##Building the Logit Model##
##For the Children Dataset##
glm.fitkids <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 + DeforestLag2, data = dfkids, family =binomial)

step.modelH22 <- stepAIC(glm.fitkids, direction = "both", trace = FALSE)
summary(step.modelH22)

car::vif(step.modelH22)

glm.fitkids1 <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego  + Temp2 + Precip2 + DeforestLag1, data = dfkids, family =binomial)

glm.fitkids2 <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag2, data = dfkids, family =binomial)

glm.fitkids3 <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag3, data = dfkids, family =binomial)

summary(glm.fitkids)
summary(glm.fitkids1)
summary(glm.fitkids2)
summary(glm.fitkids3)

##Cook's Distance##
plot(glm.fitkids, which = 4, id.n = 3)

model.data <- augment(glm.fitkids) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = H22), alpha = .5) + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Index", y="Standard Residual", color="Fever", title="Cook's Distance")

confint(glm.fitkids) ##CIs using log-likelihood##
confint.default(glm.fitkids) ##CIs using standard errors##

##Kids Deforestation 1##

step.model1 <- stepAIC(glm.fitkids1, direction = "both", trace = FALSE)
summary(step.model1)

exp(coef(step.model1))
exp(cbind(OR = coef(step.model1), confint(step.model1)))

logLik(step.modelH22)
logLik(step.model1)
logLik(glm.fitkids)

glm.probs1 <- predict(step.model1,type = "response")
glm.probs1[50:55]
mean(glm.probs1)

glm.pred1 <- ifelse(glm.probs1 > 0.15, 1, 0)

attach(dfkids)
table(glm.pred1,H22)

mean(glm.pred1 == H22)

##Kids Deforestation 2##
step.model2 <- stepAIC(glm.fitkids2, direction = "both", trace = FALSE)
summary(step.model2)

exp(coef(step.model2))
exp(cbind(OR = coef(step.model2), confint(step.model2)))

logLik(step.model2)
logLik(glm.fitkids)

glm.probs2 <- predict(step.model2,type = "response")
glm.probs2[50:55]
mean(glm.probs2)

glm.pred2 <- ifelse(glm.probs2 > 0.15, 1, 0)

attach(dfkids)
table(glm.pred2,H22)

mean(glm.pred2 == H22)

##Kids Deforestation 3##
step.model3 <- stepAIC(glm.fitkids3, direction = "both", trace = FALSE)
summary(step.model3)

exp(coef(step.model3))
exp(cbind(OR = coef(step.model3), confint(step.model3)))

logLik(step.model3)
logLik(glm.fitkids)

glm.probs3 <- predict(step.model3,type = "response")
glm.probs3[50:55]
mean(glm.probs3)

glm.pred3 <- ifelse(glm.probs3 > 0.15, 1, 0)

attach(dfkids)
table(glm.pred3,H22)

mean(glm.pred3 == H22)

##For the Adult Dataset##
##HML32##
glm.fitadultsa <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(glm.fitadultsa)

step.modelHML32 <- stepAIC(glm.fitadultsa, direction = "both", trace = FALSE)
summary(step.modelHML32)

car::vif(step.modelHML32)

glm.fitadults23a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial) 
##Left out Tomasina##

summary(glm.fitadults23a)

car::vif(glm.fitadults23a)

glm.fitadults1a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1, data = dfadults, family =binomial)

glm.fitadults2a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag2, data = dfadults, family =binomial)

glm.fitadults3a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag3, data = dfadults, family =binomial)

summary(glm.fitadults1a)
summary(glm.fitadults2a)
summary(glm.fitadults3a)

logLik(glm.fitadultsa)
logLik(glm.fitadults1a)
logLik(glm.fitadults2a)
logLik(glm.fitadults3a)

##Cook's Distance##
plot(glm.fitadultsa, which = 4, id.n = 3)

model.data <- augment(glm.fitadultsa) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = HML32), alpha = .5) + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Index", y="Standard Residual", color="Rapid Tests", title="Cook's Distance")


##Deforestation 1 Lag##
step.model1a <- stepAIC(glm.fitadults1a, direction = "both", trace = FALSE)
summary(step.model1a)

logLik(step.model1a)

exp(coef(step.model1a))
exp(cbind(OR = coef(step.model1a), confint(step.model1a)))

##Deforestation 2 Lag##
step.model2a <- stepAIC(glm.fitadults2a, direction = "both", trace = FALSE)
summary(step.model2a)

car::vif(step.model2a)

logLik(step.model2a)

exp(coef(step.model2a))
exp(cbind(OR = coef(step.model2a), confint(step.model2a)))

##Deforestation 3 Lag##
step.model3a <- stepAIC(glm.fitadults3a, direction = "both", trace = FALSE)
summary(step.model3a)

logLik(step.model3a)

exp(coef(step.model3a))
exp(cbind(OR = coef(step.model3a), confint(step.model3a)))

##HML35##
glm.fitadultsb <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(glm.fitadultsb)

step.modelHML35 <- stepAIC(glm.fitadultsb, direction = "both", trace = FALSE)
summary(step.modelHML35)

car::vif(step.modelHML35)


glm.fitadults23b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(glm.fitadults23b)

car::vif(glm.fitadults23b)


glm.fitadults1b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1, data = dfadults, family =binomial)

glm.fitadults2b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag2, data = dfadults, family =binomial)

glm.fitadults3b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag3, data = dfadults, family =binomial)

##Cook's Distance##
plot(glm.fitadultsb, which = 4, id.n = 3)

model.data <- augment(glm.fitadultsb) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = HML35), alpha = .5) + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Index", y="Standard Residual", color="Blood Smear", title="Cook's Distance")

##Deforestation 1 Lag##
step.model1b <- stepAIC(glm.fitadults1b, direction = "both", trace = FALSE)
summary(step.model1b)

logLik(step.model1b)

exp(coef(step.model1b))
exp(cbind(OR = coef(step.model1b), confint(step.model1b)))

##Deforestation 2 Lag##
step.model2b <- stepAIC(glm.fitadults2b, direction = "both", trace = FALSE)
summary(step.model2b)

logLik(step.model2b)

exp(coef(step.model2b))
exp(cbind(OR = coef(step.model2b), confint(step.model2b)))

##Deforestation 3 Lag##
step.model3b <- stepAIC(glm.fitadults3b, direction = "both", trace = FALSE)
summary(step.model3b)

logLik(step.model3b)

exp(coef(step.model3b))
exp(cbind(OR = coef(step.model3b), confint(step.model3b)))

##Log Likelihood smaller the number the better, so -3 is better than -7##

summary(glm.fitkids)
summary(glm.fitadultsa)
summary(glm.fitadultsb)

logLik(glm.fitkids)
logLik(glm.fitadultsa)
logLik(glm.fitadultsb)


logLik(step.model1b)
logLik(step.model2b)
logLik(step.model3b)

###Analyzing all the models###
summary(step.modelH22)
summary(glm.fitkids1)
summary(step.modelHML32)
summary(glm.fitadults23a)
summary(step.modelHML35)
summary(glm.fitadults23b)


logLik(step.modelH22)
logLik(glm.fitkids1)
logLik(step.modelHML32)
logLik(glm.fitadults23a)
logLik(step.modelHML35)
logLik(glm.fitadults23b)


exp(coef(step.modelH22)*.01)
exp(coef(glm.fitkids1)*.01)
exp(coef(step.modelHML32)*.01)
exp(coef(glm.fitadults23a)*.01)
exp(coef(step.modelHML35)*.01)
exp(coef(glm.fitadults23b)*.01)


##Final Models##
H22final <- glm(H22 ~ V128 + V129 + Temp + Precip2 + DeforestLag1 + Tana + Fianar + Mahaj + Toli + Diego, data = dfkids, family =binomial)
summary(H22final)


HML32final <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial) 
summary(HML32final)


HML35final <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(HML35final)

logLik(H22final)
logLik(HML32final)
logLik(HML35final)


exp(coef(H22final)*.01)
exp(coef(HML32final)*.01)
exp(coef(HML35final)*.01)


exp(cbind(OR = (coef(H22final)*.01), confint(H22final)))
exp(cbind(OR = (coef(HML32final)*.01), confint(HML32final)))
exp(cbind(OR = (coef(HML35final)*.01), confint(HML35final)))


library(jtools)
export_summs(H22final, HML32final, HML35final, confint = FALSE, scale = TRUE)
export_summs(H22final, HML32final, HML35final, exp = TRUE, confint = FALSE, scale = TRUE)

##Predictions were not necessary for this model since the research was simply trying to see if individuals in areas with higher deforestation were more likely to have malaria##

##Dependent Variables##
#Model 1 - H22 - Fever in the last 2 weeks (1 = Yes/0 = No)
#Model 2 - HML32 - Rapid Test Results (1 = Positive/0 = Negative)
#Model 3 - HML35 - Blood Smear Results (1 = Positive/0 = Negative)

##Variable Names of Interest##
#V128 & HV214 - Wall Construction (1 = Finished/0 = Unfinished)
#V129 - Floor Construction (1 = Finished/0 = Unfinished)
#HV025 - Location (1 = Urban/0 = Rural)
#HV201 - Drinking Water Source (1 = Protected/0 = Unprotected)
#HV215 - Roof Construction (1 = Finished/0 = Unfinished)
#HV253 - House sprayed in last 12 months (1 = Yes/0 = No)


