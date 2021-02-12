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
  colnames(d) <- c('a','b')
  d[, paste0(x) := ifelse(is.na(a), b, a)]
  d <- dplyr::select(d, c(x))
  dat <- dplyr::select(dat, -c(x,y)) %>% setDT()
  dat <- cbind(dat, d)
  
  return(dat)
}


Perc_Missing <- function(x){
  x <- sum(is.na(x))/length(x)*100
  return(x)
}


zeroVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  list <- which(!out > 1)
  output <- unlist(list)
  return(output)
}


DTree <- function(dat, yr){
  tree <- imputedknn %>%
  filter(year == yr) %>%
  rpart(class ~ ., dat = ., method = "class"
        , control = rpart.control(cp = 0, maxdepth = 3))
  return(tree)
}
