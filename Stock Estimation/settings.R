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
  tree <- dat %>%
  filter(year == yr) %>%
  rpart(class ~ ., dat = ., method = "class"
        , control = rpart.control(cp = 0, maxdepth = 3))
  return(tree)
}


VIF_Check <- function(dat, threshold) {
  
  set.seed(123)
  glm <- suppressWarnings(glm(class~., family = binomial(link = "logit")
                              , data = dat, control = list(maxit = 10)))
  
  
  a <- colnames(dat[,-'class'])
  all <- as.data.table(cbind(a, VIF(glm)))
  names(all) <- c('var', 'vif')
  all[, vif := as.numeric(vif)]
  all <- all[order(desc(vif))]
  max <- max(all$vif)
  print(max)
  
  while(max > threshold){
    
    set.seed(123)
    glm <- suppressWarnings(glm(class~., family = binomial(link = "logit")
                                , data = dat, control = list(maxit = 10)))
    
    
    a <- colnames(dat[,-'class'])
    all <- as.data.table(cbind(a, VIF(glm)))
    names(all) <- c('var', 'vif')
    all[, vif := as.numeric(vif)]
    all <- all[order(desc(vif))]
    max <- max(all$vif)
    print(max)
    
    if(max < threshold) break
    
    remove <- all$var[1]
    dat <- dat[, !colnames(dat) %in% remove, with=FALSE]
  }
  print(paste0('All VIFs are below threshold of ',threshold))
  return(dat)
}

# END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
