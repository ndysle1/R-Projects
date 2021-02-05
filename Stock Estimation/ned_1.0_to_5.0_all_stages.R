#Kaggle Project#
#Nick Dysle#

##########################
#2.0 - Data Understanding#
##########################
library(tidyverse)

#Loading in the data sets#
f2014 <- read_csv("data/2014_Financial_Data.csv")
f2015 <- read_csv("data/2015_Financial_Data.csv")
f2016 <- read_csv("data/2016_Financial_Data.csv")
f2017 <- read_csv("data/2017_Financial_Data.csv")
f2018 <- read_csv("data/2018_Financial_Data.csv")

#Adding variable 'Year' to each data set since this variable is not included in the original data sets# 
f2014$Year <- 2014
f2015$Year <- 2015
f2016$Year <- 2016
f2017$Year <- 2017
f2018$Year <- 2018

dim(f2014)
dim(f2015)
dim(f2016)
dim(f2017)
dim(f2018)

#Comparing data sets to find any glaring differences and found that not all variable names match up#
library(arsenal)
summary(comparedf(f2014, f2015), max.print.obs = 1)
summary(comparedf(f2014, f2016), max.print.obs = 1)
summary(comparedf(f2014, f2017), max.print.obs = 1)
summary(comparedf(f2014, f2018), max.print.obs = 1)

summary(comparedf(f2015, f2016), max.print.obs = 1)
summary(comparedf(f2015, f2017), max.print.obs = 1)
summary(comparedf(f2015, f2018), max.print.obs = 1)

summary(comparedf(f2016, f2017), max.print.obs = 1)
summary(comparedf(f2016, f2018), max.print.obs = 1)
summary(comparedf(f2017, f2018), max.print.obs = 1)

#Doing a secondary check of variable names to verify that they do not match#
a <- colnames(f2014)
b <- colnames(f2015)
c <- colnames(f2016)
d <- colnames(f2017)
e <- colnames(f2018)

setdiff(e,a)
setdiff(c,b)

#Renaming the variable names that differ between the datasets so they all match f2014#
f2015 <- rename(f2015, "Stock" = "X1")
f2016 <- rename(f2016, "Stock" = "X1")
f2017 <- rename(f2017, "Stock" = "X1")
f2018 <- rename(f2018, "Stock" = "X1", "nextYR_PRICE_VAR" = "2019 PRICE VAR [%]")

#Combining the 5 datasets into a single data set for exploratory analysis#
data <- rbind(f2014, f2015, f2016, f2017, f2018)

#Reordering the variables to pull dependent variables and important independent variables to the front of the dataset (personal preference)#
data <- data %>%
  select(1, 224, 225, 226, 223, everything())

#Checking the structure of the data set#
str(data)

#Finding how many variables fall into each data type#
table(sapply(data, class))

#Checking NA values#
na <- apply(is.na(data),2,sum)

na <- as.data.frame(na)
print(na)

data <- subset(data, select = -c(operatingCycle, cashConversionCycle)) #Both columns are NA so removed them#

names(data)

#Saving combined data set as CSV#
write_csv(data, "data/combined_financial.csv")

#Importing combined data set#
data <- read_csv("data/combined_financial.csv")

#Statistical Description#
summary(data)
view(summary(data))

library(psych)
describe(data)
view(describe(data))

?describe
#Correlation#
library(corrplot)

cor <- cor(na.omit(data[,c(6:224)]))

corrplot(cor, tl.pos = "n", method = "color")

colors = colorRampPalette(c("yellow", "orange", "red")) (20)
heatmap(cor, Rowv = NA, Colv = NA, col = colors, labRow = FALSE, labCol = FALSE)

#Checking to see if any variables are highly correlated with dependent variables#
#ML#
ML <- na.omit(data[,c(2,6:224)])
corr <- abs(round(cor(ML),4))
corr <- (corr[, c(1)])
corr <- sort(corr, decreasing = TRUE)
corr

as_tibble(corr) %>%
  top_n(10)

#LT#
LT <- na.omit(data[,c(3,6:224)])
corr <- abs(round(cor(LT),4))
corr <- (corr[, c(1)])
corr <- sort(corr, decreasing = TRUE)
corr

as_tibble(corr) %>%
  top_n(10)

#Variance#
library(raster)

df <- na.omit(data[ -c(1,5)])

min <- apply(df,2, min)
max <- apply(df,2, max)
mean <- apply(df,2, mean)
var <- apply(df,2,var)
sd <- apply(df, 2, sd)
quant <- apply(df,2,quantile)
cv <- apply(df,2,cv)    #coefficient of variation of less than 1 is low#
  
rows <- c("min", "max", "mean", "var", "sd", "0%", "25%", "50%", "75%", "100%", "cv")
var.table <- rbind(t(min), t(max),t(mean), t(var), t(sd), quant, t(cv))
rownames(var.table) = rows
view(var.table)
write.csv(as.data.frame(var.table), "data/summary_stats.csv", row.names = TRUE)

#Outliers#
par(mfrow=c(3,3))

for (i in colnames(df)) {
  x <- df[,i]
  boxplot(x, main=colnames(x))}

par(mfrow=c(1,1))

#Z scores#
library(outliers)

z <- scores(df, type = "z")
summary(z)

t(apply(z,2,range))

outlier <- abs(z) > 3
table(outlier)

#Distributions#
library(Hmisc)

par(mfrow=c(3,3))

hist.data.frame(df)

par(mfrow=c(1,1))

hist(df$grossProfitMargin)
hist(df$Revenue)

########################
#3.0 - Data Preparation#
########################
data <- read_csv("data/combined_financial.csv")
names(data)

#Removing columns with the same sum (duplicates)
dups <- data[ , which(duplicated(t(data)))]
names(dups)

data <- data[ , which(!duplicated(t(data)))]
view(data)

#Gives the same result as duplicated()
#c <- colSums((data[,c(6:214)]), na.rm = TRUE)
#c <- c[duplicated(c)]
#view(c)

#Looking for missing values#
na <- apply(is.na(data),2,sum)
max(na)
print(na)
sort(na, decreasing = TRUE)
head(sort(na, decreasing = TRUE), n=25)

#Merging and dropping duplicated variable names
view(data[, c("Payout Ratio", "payoutRatio")])
data <- {x <- "Payout Ratio"
  y <- "payoutRatio"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- d[ , -c(1:2)]
  data <- cbind(data, d)
}

view(data[, c("interestCoverage", "Interest Coverage")])
data <- {x <- "Interest Coverage"
  y <- "interestCoverage"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("netProfitMargin", "Net Profit Margin")])
data <- {x <- "Net Profit Margin"
  y <- "netProfitMargin"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("Dividend Yield", "dividendYield")])
data <- {x <- "Dividend Yield"
  y <- "dividendYield"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("PE ratio", "priceEarningsRatio")])
data <- {x <- "PE ratio"
  y <- "priceEarningsRatio"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("priceToFreeCashFlowsRatio", "PFCF ratio")])
data <- {x <- "PFCF ratio"
  y <- "priceToFreeCashFlowsRatio"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("priceToOperatingCashFlowsRatio", "POCF ratio")]) #??????
data <- {x <- "POCF ratio"
  y <- "priceToOperatingCashFlowsRatio"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("priceToSalesRatio", "Price to Sales Ratio")])
data <- {x <- "Price to Sales Ratio"
  y <- "priceToSalesRatio"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("Days Payables Outstanding", "daysOfPayablesOutstanding")])
data <- {x <- "Days Payables Outstanding"
  y <- "daysOfPayablesOutstanding"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("Free Cash Flow per Share", "freeCashFlowPerShare")])
data <- {x <- "Free Cash Flow per Share"
  y <- "freeCashFlowPerShare"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("ROE", "returnOnEquity")])
data <- {x <- "ROE"
  y <- "returnOnEquity"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("priceToBookRatio", "PTB ratio")])
data <- {x <- "PTB ratio"
  y <- "priceToBookRatio"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("priceBookValueRatio", "PB ratio")])
data <- {x <- "PB ratio"
  y <- "priceBookValueRatio"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("operatingCashFlowPerShare", "Operating Cash Flow per Share")])
data <- {x <- "Operating Cash Flow per Share"
  y <- "operatingCashFlowPerShare"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

view(data[, c("Cash per Share", "cashPerShare")])
data <- {x <- "Cash per Share"
  y <- "cashPerShare"
  d <- data[, c(x,y)]
  data <- select(data, -c(x,y))
  d$c <- ifelse(is.na(d[[1]]), round(d[[2]],4), d[[1]])
  d$c <- ifelse(d[[3]] == 0.0000 & d[[2]] != 0.0000, round(d[[2]],4), d[[3]])
  colnames(d)[3] <- x
  d <- subset(d, select = -c(1:2))
  data <- cbind(data, d)
}

#Fixing variable names
library(stringr)
names(data) <- str_trim(names(data), side = "both")
names(data) <- str_to_lower(names(data), locale = "en")
names(data) <- str_replace_all(names(data), " ", "_")
names(data) <- str_replace_all(names(data), "-", "")
names(data)

#Categorical Encoding
data$sector <- as.factor(data$sector)
data$sector_num <- as.numeric(data$sector)

#Reordering data to put "sector" with "sector_num"
data <- data %>%
  select(1:5, 200, everything())

#Looking for missing values#
na <- apply(is.na(data),2,sum)
max(na)
print(na)
sort(na, decreasing = TRUE)
head(sort(na, decreasing = TRUE), n=25)

#Checking how many cases are complete
sum(complete.cases(data))

#Checking for NA across rows
data$na <- rowSums(is.na(data))
max(data$na)
head(sort(data$na, decreasing = TRUE),n = 20)
summary(data$na)

#Found that 50 was a good cut off for dropping
drop <- data %>% 
  filter(na >= 50)

data <- data %>%
  filter(na <= 50)
data <- subset(data, select = -na)

#Re-checking the NAs across columns
na <- apply(is.na(data),2,sum)
max(na)
print(na)
sort(na, decreasing = TRUE)
head(sort(na, decreasing = TRUE), n=25)

#Keeping only columns with less than ~25 percent missing
perc_miss <- function(x){sum(is.na(x))/length(x)*100}
perc <- apply(data,2,perc_miss)
max(perc)
print(perc)
sort(perc, decreasing = TRUE)
head(sort(perc, decreasing = TRUE), n=25)

#Choosing to keep only variables with less than 15% missing data
data <- data[, which(apply(data,2,perc_miss) < 15.0)]

#Imputing the remaining NAs#
library(missForest)
library(imputeMissings)

missing <- data[,-c(1:7)]

rm(list=setdiff(ls(), c("data", "missing")))

#imputedrf <- impute(missing, method = "randomForest")
imputedm <- impute(missing, method = "median/mode")

#summary(imputedrf)
summary(imputedm)

#Combining the sets with the original data
#imputedrf <- cbind(data[,c(1:7)], imputedrf)
imputedm <- cbind(data[,c(1:7)], imputedm)
#dim(imputedrf)
dim(imputedm)

#na <- apply(is.na(imputedrf),2,sum)
#max(na)

na <- apply(is.na(imputedm),2,sum)
max(na)

#Saving data sets to CSV
write_csv(data, "data/clean_financial.csv")
write_csv(imputedm, "data/imputedm_financial.csv")
#write_csv(imputedrf, "data/imputedrf_financial.csv")  

####################
#4.0 - Data Modeling
####################

#Loading main package, setting working directory, and loading in data sets
library(tidyverse)
setwd("C:/Users/User/Documents/nick-kaggle-training")
data <- read_csv("data/clean_financial.csv") 
imputedm <- read_csv("data/imputedm_financial.csv")




