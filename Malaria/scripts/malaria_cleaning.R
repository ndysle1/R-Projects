
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Data Cleaning #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# This script cleans the datasets and prepares 
# them for the analysis and modeling scripts


## Packages/Functions ----
source('Malaria/functions/malaria_functions.R')
library(readxl)


## Loading Data ----
import <- list.files(paste0(dir$data))

for(i in import){
  
  if (grepl('.csv',i) == TRUE) {
    assign(gsub("\\..*","",i), fread(paste0(dir$data,i)))
  } else {
    assign(gsub("\\..*","",i)
           , read_xlsx(paste0(dir$data,i),sheet = 1))
  }
}


## Combine Malaria Datasets ----
dfadults <- rbind(df2011pr, df2013pr, df2016pr)
dfkids <- rbind(df2011f, df2013f, df2016f)

rm(df2011pr, df2013pr, df2016pr,
   df2011f, df2013f, df2016f)


## Merge Climate Data ----
dfadults <- merge(x = dfadults, y = climate
                  , by.x=c("HV006", "HV007", "SHPROV")
                  , by.y=c("Month", "Year", "Province"))
dfkids <- merge(x = dfkids, y = climate
                , by.x=c("V006", "V007", "SPROV")
                , by.y=c("Month", "Year", "Province"))

rm(climate)


## Merge Deforestation Data ----
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


## Data Cleaning ----
# Removing inconclusive results and NA values from dependent variables
dfadults <- subset(dfadults, HML35 != 6 & HML32 != 7 & HV253 != 8) 
dfadults <- subset(dfadults, HML32 != 6)
dfadults <- na.omit(dfadults)

dfkids <- subset(dfkids, H22 != 8) 
dfkids <- na.omit(dfkids)

# Creating precipitation & temperature values
# Taken from previous scientific findings on the matter
setDT(dfadults)
setDT(dfkids)

dfadults[, Precip2 := (Precip^2)]
dfkids[, Precip2 := (Precip^2)]

dfadults[, Temp2 := (Temp^2)]
dfkids[, Temp2 := (Temp^2)]

# Creating province dummy variables by name
dfadults[, Tana   :=  ifelse(SHPROV==1,1,0)]
dfadults[, Fianar :=  ifelse(SHPROV==2,1,0)]
dfadults[, Tomas  :=  ifelse(SHPROV==3,1,0)]
dfadults[, Mahaj  :=  ifelse(SHPROV==4,1,0)]
dfadults[, Toli   :=  ifelse(SHPROV==5,1,0)]
dfadults[, Diego  :=  ifelse(SHPROV==7,1,0)]

dfkids[, Tana   :=  ifelse(SPROV==1,1,0)]
dfkids[, Fianar :=  ifelse(SPROV==2,1,0)]
dfkids[, Tomas  :=  ifelse(SPROV==3,1,0)]
dfkids[, Mahaj  :=  ifelse(SPROV==4,1,0)]
dfkids[, Toli   :=  ifelse(SPROV==5,1,0)]
dfkids[, Diego  :=  ifelse(SPROV==7,1,0)]

# Grouping independent variables to simplify outcomes
# Example: rather than listing all water sources, converting
# variable to protected vs unprotected water sources
dfadults[, HV201 := ifelse(HV201 %in% c(32,40,42,43,51),0,1)]
dfadults[, HV205 := ifelse(HV205 %in% c(30,31,41,42,43,96),0,1)]
dfadults[, HV228 := ifelse(HV228 %in% c(0,3), 0,1)]
dfadults[, HV213 := ifelse(HV213 %in% c(10,11,12,21,22,23),0,1)]
dfadults[, HV214 := ifelse(HV214 %in% c(10,11,12,21,22,23),0,1)]
dfadults[, HV215 := ifelse(HV215 %in% c(10,11,12,13,20,21,22,23,24),0,1)]
dfadults[, HV025 := ifelse(HV025 %in% c(2),0,1)]

dfkids[, V113 := ifelse(V113 %in% c(32,40,42,43,51),0,1)]
dfkids[, V116 := ifelse(V116 %in% c(30,31,41,42,43,96,97),0,1)]
dfkids[, V460 := ifelse(V460 %in% c(0,3), 0,1)]
dfkids[, V127 := ifelse(V127 %in% c(10,11,12,21,22,23),0,1)]
dfkids[, V128 := ifelse(V128 %in% c(10,11,12,13,20,21,22,23,24,25),0,1)]
dfkids[, V129 := ifelse(V129 %in% c(10,11,12,13,20,21,22,23,24),0,1)]
dfkids[, V025 := ifelse(V025 %in% c(2),0,1)]

str(dfadults)
str(dfkids)

# Converting certain variables to factors
dfadults <- dfadults %>%
  mutate_at(c("SHPROV","HV024", "HV006", "HV007", "HV025"
              , "HV201", "HV205", "HV213", "HV214", "HV215"
              , "HV228", "HV253", "HML32","HML35"), as.factor)

dfkids <- dfkids %>%
  mutate_at(c("SPROV","V024", "V006", "V007", "V025"
              , "V113", "V116", "V127", "V128", "V129"
              , "V460", "H22"), as.factor)

summary(dfkids$V113)
summary(dfadults$HV025)


## Write to CSV ----
fwrite(dfadults, paste0(dir$final, 'dfadults.csv'))
fwrite(dfkids, paste0(dir$final, 'dfkids.csv'))

# END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
