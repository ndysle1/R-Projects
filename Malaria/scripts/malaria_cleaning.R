
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Data Cleaning #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# This script cleans the datasets and prepares them for the analysis
# and modeling scripts


# Packages/Functions ----
source('Malaria/functions/malaria_functions.R')
library(readxl)

# Loading Data ----
import <- list.files(paste0(dir$data))

for(i in import){
  
  if (grepl('.csv',i) == TRUE) {
    assign(gsub("\\..*","",i), fread(paste0(dir$data,i)))
  } else {
    assign(gsub("\\..*","",i), read_xlsx(paste0(dir$data,i),sheet = 1))
  }
}


# Combine Malaria Datasets ----
dfadults <- rbind(df2011pr, df2013pr, df2016pr)
dfkids <- rbind(df2011f, df2013f, df2016f)

rm(df2011pr, df2013pr, df2016pr,
   df2011f, df2013f, df2016f)


# Merge Climate Data ----
dfadults <- merge(x = dfadults, y = climate, by.x=c("HV006", "HV007", "SHPROV"), by.y=c("Month", "Year", "Province"))
dfkids <- merge(x = dfkids, y = climate, by.x=c("V006", "V007", "SPROV"), by.y=c("Month", "Year", "Province"))

rm(climate)


# Merge Deforestation Data ----
# Create Lags
deforestation <- Create_Lags(data = deforestation)

# Left join to malaria datasets                      
dfadults <- left_join(x = dfadults, y = deforestation
                      , by = c("SHPROV" = "Province"
                      , "HV024" = "Region"
                      , "HV007" = "Year"))

dfkids <- left_join(x = dfkids, y = deforestation
                    , by = c("SPROV" = "Province"
                    , "V024" = "Region"
                    , "V007" = "Year"))


# Data Cleaning ----
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
