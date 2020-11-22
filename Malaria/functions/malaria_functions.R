# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Functions #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Packages ----
library(tidyverse)
library(data.table)


# Directories ----
dir <- list()
dir$project <- 'C:/Users/User/Documents/GitHub/R-Projects/Malaria/' 
dir$scripts <- paste0(dir$project,'scripts/')
dir$data    <- paste0(dir$project,'data/')
dir$final   <- paste0(dir$project,'final_data/')

# Functions ----
Create_Lags <- function(data = deforestation){
  
  deforestation <- deforestation %>%
    select(Province, Region, PercLoss2008, PercLoss2009
           , PercLoss2010, PercLoss2011, PercLoss2012
           , PercLoss2013, PercLoss2014, PercLoss2015, PercLoss2016)
  
  deforestation <- gather(deforestation, Year, Loss, PercLoss2008:PercLoss2016) %>%
    mutate(Year = as.integer(str_remove_all(Year, 'PercLoss'))) %>%
    arrange(Province, Region, Year) %>%
    mutate(DeforestLag3 = lag(Loss, n=3)
           ,DeforestLag2 = lag(Loss, n=2)
           ,DeforestLag1 = lag(Loss, n=1)) %>%
    select(-c(Loss))
  
  return(deforestation)
  
}
