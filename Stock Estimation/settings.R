# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#  Stock Estimation Settings & Functions  #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Packages ----
library(tidyverse)
library(data.table)


## Directories ----
dir <- list()
dir$project    <- 'C:/Users/User/Documents/GitHub/R-Projects/Stock Estimation/' 
dir$notebooks  <- paste0(dir$project,'notebooks/')
dir$data       <- paste0(dir$project,'data/')
dir$final_data <- paste0(dir$project,'final_data/')


## Functions ----
Name_Changer <- function(x,y,dat){
  d <- dplyr::select(dat, c(x,y)) %>% setDT()
  dat <- dplyr::select(dat, -c(x,y)) %>% setDT()
  d <- d[, x := fifelse(is.na(x), y, x)]
  d <- d[, x := ifelse(is.na(x), round(x,4), x)]
  d <- dplyr::select(d, -c(1:2)) %>% setDT()
  setnames(d, 'x', paste0(x))
  dat <- cbind(dat, d)
  return(dat)
}


Perc_Missing <- function(x){
  x <- sum(is.na(x))/length(x)*100
  return(x)
}
