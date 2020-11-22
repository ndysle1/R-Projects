# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Data Protection #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# This script takes the original datasets and pulls only needed variables
# This script does not work on GitHub because the datasets contain PII
# and can not be shared with the public at-large


# Packages/Functions ----
source('Malaria/functions/malaria_functions.R')
library(haven)


# Importing Original Data ----
PII <- list.files(paste0(dir$project,'PII/'))

for(i in PII){
  
  assign(gsub("\\..*","",i), read_sas(paste0(dir$project,'PII/',i), NULL))
  
}
# Original Datasets
# df2011pr <- 40581 obs. of 283 variables
# df2013pr <- 38776 obs. of 341 variables
# df2016pr <- 49141 obs. of 352 variables
# df2011f  <- 6248 obs. of 941 variables
# df2013f  <- 5477 obs. of 998 variables
# df2016f  <- 6978 obs. of 997 variables


# Data Transformation ----
# The 2016 surveys did not include provinces
# Only regions were provided so provinces had to be created

# Creating SHPROV for 2016 adults dataset
df2016pr[, SHPROV := case_when(
      HV024 < 19                ~ 1
      HV024 >= 20 & HV024 < 29  ~ 2
      HV024 >= 30 & HV024 < 39  ~ 3
      HV024 >= 40 & HV024 < 49  ~ 4
      HV024 >= 50 & HV024 < 59  ~ 5
      HV024 >= 70 & HV024 < 79  ~ 7
)]

# Creating SPROV for 2016 kids dataset
df2016f[, SPROV := case_when(
      V024 < 19               ~ 1
      V024 >= 20 & V024 < 29  ~ 2
      V024 >= 30 & V024 < 39  ~ 3
      V024 >= 40 & V024 < 49  ~ 4
      V024 >= 50 & V024 < 59  ~ 5
      V024 >= 70 & V024 < 79  ~ 7
)]


# Reducing Variables ----
# Selecting only the needed variables
# This removes all PII information

# Adult datasets
adults <- c('HV006', 'HV007', 'HV024', 'HV025', 'HV201'
            , 'HV205', 'HV213', 'HV214', 'HV215', 'HV228'
            , 'HV253', 'SHPROV', 'HML32', 'HML35')

df2011pr <- df2011pr %>%
  select(c(adults))
df2013pr <- df2013pr %>%
  rename(HV024 = SHREGION1) %>%
  select(c(adults))
df2016pr <- df2016pr %>%
  select(c(adults))

# Kids datasets
kids <- c('V006', 'V007', 'V024', 'V025', 'V113', 'V116'
          , 'V127', 'V128', 'V129', 'V460', 'H22', 'SPROV')

df2011f <- df2011f %>%
  select(c(kids))
df2013f <- df2013f %>%
  rename(V024 = SREGION) %>%
  select(c(kids))
df2016f <- df2016f %>%
  select(c(kids))


# Write to CSV ----
to_write <- c('df2011f', 'df2011pr', 'df2013f'
              , 'df2013pr', 'df2016f', 'df2016pr')

for(i in to_write){
  
  write.csv(i, paste0(dir$data, i, '.csv'), row.names = FALSE)
  
}

# END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~