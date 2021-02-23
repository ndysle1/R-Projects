# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Exploratory Analysis #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# This script takes performs exploratory analysis 


## Packages/Functions ----
source('Malaria/functions/malaria_functions.R')


## Loading Data ----
dfadults <- fread(paste0(dir$final,'dfadults.csv'))
dfkids   <- fread(paste0(dir$final,'dfkids.csv'))


## Exploratory Analysis ----
names(dfadults)
names(dfkids)

str(dfkids)
str(dfadults)

summary(dfadults$SHPROV)
summary(dfkids$SPROV)

apply(is.na(dfkids),2,sum)
apply(is.na(dfadults),2,sum)

# Simple histoograms 
dfadults %>%
  group_by(HV025) %>%
  summarise(count = n()) %>%
  ggplot(aes(HV025, count)) +
  geom_col()

dfadults %>%
  group_by(HV025, SHPROV) %>%
  summarise(count = n()) %>%
  ggplot(aes(SHPROV, count)) +
  geom_col()

dfkids %>%
  group_by(V025) %>%
  summarise(count = n()) %>%
  ggplot(aes(V025, count)) +
  geom_col()

dfkids %>%
  group_by(V025, SPROV) %>%
  summarise(count = n()) %>%
  ggplot(aes(SPROV, count)) +
  geom_col()

## Correlation Matrix ----
library(corrplot)

resadults <- cor(dfadults[,c(15,16,17,18,19,20,21)])
round(resadults, 2)

reskids <- cor(dfkids[,c(13,14,15,16,17,18,19)])
round(reskids, 2)

# Correlation Graph
corrplot(resadults, method = "circle")
corrplot(reskids, method = "circle")

## Graphing Dependent Variables ----
# Blood Smears
dfadults %>%
  group_by(SHPROV, HML32) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(SHPROV)
             ,y = count
             , fill = as.factor(HML32))) + 
  geom_col() +
  labs(x="Provinces"
       , y="Individuals"
       , title = "Blood Smear Results") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Result"
                      ,labels=c("Negative", "Positive")) +
  scale_x_discrete(breaks = c(1,2,3,4,5,7)
                     ,labels=c("ANTA", "FIAN", "TOAM"
                               , "MAHA","TOLI","ANTS"))

# Rapid Tests
dfadults %>%
  group_by(SHPROV, HML35) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(SHPROV)
             ,y = count
             , fill = as.factor(HML35))) + 
  geom_col() +
  labs(x="Provinces"
       , y="Individuals"
       , title = "Rapid Test Results") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Result"
                      ,labels=c("Negative", "Positive")) +
  scale_x_discrete(breaks = c(1,2,3,4,5,7)
                     ,labels=c("ANTA", "FIAN", "TOAM"
                               , "MAHA","TOLI","ANTS"))

# Fevers
dfkids %>%
  group_by(SPROV, H22) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(SPROV)
             , y = count
             , fill = as.factor(H22))) + 
  geom_col() +
  labs(x="Provinces"
       , y="Individuals"
       , title = "Fever in Last Two Weeks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Fever"
                      ,labels=c("No", "Yes")) +
  scale_x_discrete(breaks = c(1,2,3,4,5,7)
                     ,labels=c("ANTA", "FIAN", "TOAM"
                               , "MAHA","TOLI","ANTS"))

# Malaria rates per province and testing type
tapply(as.numeric(dfadults$HML32)-1,dfadults$SHPROV,mean)
tapply(as.numeric(dfadults$HML35)-1,dfadults$SHPROV,mean)
tapply(as.numeric(dfkids$H22)-1,dfkids$SPROV,mean)

fever <- factor(dfadults$HML32, c(0,1))
Prov <- factor(dfadults$SHPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

fever <- factor(dfadults$HML35, c(0,1))
Prov <- factor(dfadults$SHPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

fever <- factor(dfkids$H22, c(0,1))
Prov <- factor(dfkids$SPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

# END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
