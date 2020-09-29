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

##Importing datasets##
df2011pr <- read_sas("mis2011fpr.SAS7BDAT", NULL)
df2013pr <- read_sas("mis2013fpr.SAS7BDAT", NULL)
df2016pr <- read_sas("mis2016fpr.SAS7BDAT", NULL)
df2011f <- read_sas("mis2011f.SAS7BDAT", NULL)
df2013f <- read_sas("mis2013f.SAS7BDAT", NULL)
df2016f <- read_sas("mis2016f.SAS7BDAT", NULL)
climate_data <- read_excel("Climate Data.xlsx", sheet = 1)
deforest_data <- read_excel("Deforestation Data.xlsx", sheet = 2)
##wbank_data <- read_excel("World Bank Data.xls", sheet = 1) no longer using since at national level##

##Fixing 2016 dataset to include Provinces##

df2016pr$SHPROV[df2016pr$HV024<19]<-1
df2016pr$SHPROV[df2016pr$HV024>=20 & df2016pr$HV024<29]<-2
df2016pr$SHPROV[df2016pr$HV024>=30 & df2016pr$HV024<39]<-3
df2016pr$SHPROV[df2016pr$HV024>=40 & df2016pr$HV024<49]<-4
df2016pr$SHPROV[df2016pr$HV024>=50 & df2016pr$HV024<59]<-5
df2016pr$SHPROV[df2016pr$HV024>=70 & df2016pr$HV024<79]<-7

##Selecting only the needed variables##
df2011pr = df2011pr %>%
  select(HHID, HV006, HV007, HV024, HV025, HV201, HV205, HV213, HV214, HV215, HV228, HV253, SHPROV, HML32, HML35)
df2013pr = df2013pr %>%
  select(HHID, HV006, HV007, SHREGION1, HV025, HV201, HV205, HV213, HV214, HV215, HV228, HV253, SHPROV, HML32, HML35)
df2016pr = df2016pr %>%
  select(HHID, HV006, HV007, HV024, HV025, HV201, HV205, HV213, HV214, HV215, HV228, HV253, SHPROV, HML32, HML35)

##Adding in Climate Data##
df2011pr <- merge(x = df2011pr, y = climate_data, by.x=c("HV006", "HV007", "SHPROV"), by.y=c("Month", "Year", "Province"))
df2013pr <- merge(x = df2013pr, y = climate_data, by.x=c("HV006", "HV007", "SHPROV"), by.y=c("Month", "Year", "Province"))
df2016pr <- merge(x = df2016pr, y = climate_data, by.x=c("HV006", "HV007", "SHPROV"), by.y=c("Month", "Year", "Province"))

##Fixing deforestation data##
deforest_data <- deforest_data %>%
  select(Province, Region, PercLoss2008, PercLoss2009, PercLoss2010, PercLoss2011, PercLoss2012, PercLoss2013, PercLoss2014, PercLoss2015, PercLoss2016)

##Adding in Deforestation Data##
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



##AT THIS STEP ALL THREE DATASETS FOR THE YEARS OF THE STUDY ARE COMPLETE##



##Creating subsets of dependent variables for each of the years##
##Blood_smear = subset (df2011pr, HML32 == 1)
##Blood_smear2 = subset (df2011pr, HML32 == 0)
##Rapid_test = subset (df2011pr, HML35 == 1)
##Rapid_test2 = subset (df2011pr, HML35 == 0)
##Blood_smear = subset (df2013pr, HML32 == 1)
##Blood_smear2 = subset (df2013pr, HML32 == 0)
##Rapid_test = subset (df2013pr, HML35 == 1)
##Rapid_test2 = subset (df2013pr, HML35 == 0)
##Blood_smear = subset (df2016pr, HML32 == 1)
##Blood_smear2 = subset (df2016pr, HML32 == 0)
##Rapid_test = subset (df2016pr, HML35 == 1)
##Rapid_test2 = subset (df2016pr, HML35 == 0)

##For children data sets, these are variables of interest##
df2016f$SPROV[df2016f$V024<19]<-1
df2016f$SPROV[df2016f$V024>=20 & df2016f$V024<29]<-2
df2016f$SPROV[df2016f$V024>=30 & df2016f$V024<39]<-3
df2016f$SPROV[df2016f$V024>=40 & df2016f$V024<49]<-4
df2016f$SPROV[df2016f$V024>=50 & df2016f$V024<59]<-5
df2016f$SPROV[df2016f$V024>=70 & df2016f$V024<79]<-7


df2011f = df2011f %>%
  select(CASEID, V006, V007, V024, V025, V113, V116, V127, V128, V129, V460, H22, SPROV)
df2013f = df2013f %>%
  select(CASEID, V006, V007, SREGION, V025, V113, V116, V127, V128, V129, V460, H22, SPROV)
df2016f = df2016f %>%
  select(CASEID, V006, V007, V024, V025, V113, V116, V127, V128, V129, V460, H22, SPROV)

##Adding in Climate Data to Children Datasets##
df2011f <- merge(x = df2011f, y = climate_data, by.x=c("V006", "V007", "SPROV"), by.y=c("Month", "Year", "Province"))
df2013f <- merge(x = df2013f, y = climate_data, by.x=c("V006", "V007", "SPROV"), by.y=c("Month", "Year", "Province"))
df2016f <- merge(x = df2016f, y = climate_data, by.x=c("V006", "V007", "SPROV"), by.y=c("Month", "Year", "Province"))

##Adding in Deforestation Data##
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

##Checking number of Fever variables in children the past 2 weeks##
##FeverPos = subset(df2011f, H22 == 1)
##FeverNeg = subset(df2011f, H22 == 0)
##FeverPos = subset(df2013f, H22 == 1)
##FeverNeg = subset(df2013f, H22 == 0)
##FeverPos = subset(df2016f, H22 == 1)
##FeverNeg = subset(df2016f, H22 == 0)

##Rename Region Variables in 2013 to match 2011 and 2016##

colnames(df2013pr)[2] <- "HV024"
colnames(df2013f)[2] <- "V024"

##Combining all 3 years of datasets for both kids and adults datasets##

dfadults <- rbind(df2011pr, df2013pr, df2016pr)
dfkids <- rbind(df2011f, df2013f, df2016f)

##Amending variables##
dfkids <- subset(dfkids, H22 != 8) 
dfkids <- na.omit(dfkids)
dfadults <- subset(dfadults, HML35 != 6 & HML32 != 7 & HV253 != 8) 
dfadults <- subset(dfadults, HML32 != 6)

dfadults <- na.omit(dfadults)

str(dfkids)
str(dfadults)

dfkids$Precip2 <- (dfkids$Precip)^2
dfadults$Precip2 <- (dfadults$Precip)^2

dfkids$Temp2 <- (dfkids$Temp)^2
dfadults$Temp2 <- (dfadults$Temp)^2

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

##Save to CSV##

write.csv(dfkids, file="dfkids.csv")
write.csv(dfadults, file="dfadults.csv")


##Combining Variables so there are only 2 options##
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


##Changing variables to correct data type""
dfkids <- mutate_at(dfkids, c("SPROV","V024", "V025", "V113", "V116", "V127", "V128", "V129", "V460", "H22"), as.factor)
dfadults <- mutate_at(dfadults, c("SHPROV","HV024", "HV025", "HV201", "HV205", "HV213", "HV214", "HV215", "HV228", "HV253", "HML32","HML35"), as.factor)

summary(dfkids$V113)

##Plotting##
str(dfkids)
str(dfadults)

summary(dfkids$Temp)

apply(is.na(dfkids),2,sum)
apply(is.na(dfadults),2,sum)

M<-cor(dfkids[,c("Temp","Precip","DeforestLag3", "DeforestLag2", "DeforestLag1")])
corrplot(M, method = "circle")

l <- ggplot(dfkids, aes(V025,fill=H22))
l <- l + geom_histogram(stat="count")
print(l)

l <- ggplot(dfkids, aes(SPROV,fill=H22))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(dfkids$H22)-1,dfkids$SPROV,mean)

Rapid <- subset(dfkids, H22 == 1) 
Rapid <- subset(dfadults, HML35 ==6)


####END OF USEFUL CODE - THE REST IS JUST NOTES AND POSSIBILE FUTURE NEEDS####
##Saving dataset to R##
saveRDS(dfkids, file="dfkids.rds")
saveRDS(dfadults, file="dfadults.rds")

#Code to potentially export dataset to Excel##
library("xlsx")

write.xlsx(x, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
