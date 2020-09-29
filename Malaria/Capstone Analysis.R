library(dplyr)
library(haven)
library(readxl)
library(tidyverse)
library(reshape)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(car)
library(caTools)
library(e1071)
library(glmnet)
library(ISLR)
library(leaps)
library(caret)
library(MASS)
library(Hmisc)
library(broom)
library(aod)
library(stargazer)
library(huxtable)

##Importing Cleaned Datasets##
dfkids <- readRDS("dfkids.rds", refhook = NULL)
dfadults <- readRDS("dfadults.rds", refhook = NULL)

##Plotting##
str(dfkids)
str(dfadults)

##Descriptive Statistics Tables##

names(dfadults)

X <- dfkids$H22

sd(X)
summary(X)

##Plotting and analyzing datasets##
summary(dfadults$SHPROV)

apply(is.na(dfkids),2,sum)
apply(is.na(dfadults),2,sum)

##Correlation Matrix##
my_data <- dfkids[,c(14,15,16,17,18,19)]

res <- cor(my_data)
round(res, 2)

##Correlation Graph##
M<-cor(dfkids[,c("Temp","Precip", "Precip2","DeforestLag3", "DeforestLag2", "DeforestLag1")])
corrplot(M, method = "circle")

M<-cor(dfadults[,c("Temp","Precip", "Precip2","DeforestLag3", "DeforestLag2", "DeforestLag1")])
corrplot(M, method = "circle")

##Add this to your paper??##
x <- dfkids[,14:20]
y <- dfkids[,13]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

x <- dfadults[,16:22]
y <- dfkids[,14]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

x <- dfadults[,16:22]
y <- dfkids[,15]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


##Graphing Dependent Variables##
ggplot(dfkids, aes(V025,fill=H22)) + geom_histogram(stat="count")

ggplot(dfadults, aes(HV025,fill=HML35)) + geom_histogram(stat="count")

ggplot(dfadults, aes(HV025,fill=HML32))+ geom_histogram(stat="count")

ggplot(dfkids, aes(SPROV,fill=H22)) + geom_histogram(stat="count") + labs(x="Provinces", y="Individuals", title = "Fever in Last Two Weeks")+ theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name="Fever",labels=c("No", "Yes"))+ scale_x_discrete(breaks=c("1","2","3","4","5","7"),labels=c("ANTA", "FIAN", "TOAM", "MAHA","TOLI","ANTS"))

ggplot(dfadults, aes(SHPROV,fill=HML32))+ geom_histogram(stat="count") + labs(x="Provinces", y="Individuals", title = "Blood Smear Results")+ theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name="Result",labels=c("Negative", "Positive"))+ scale_x_discrete(breaks=c("1","2","3","4","5","7"),labels=c("ANTA", "FIAN", "TOAM", "MAHA","TOLI","ANTS"))

ggplot(dfadults, aes(SHPROV,fill=HML35))+ geom_histogram(stat="count") + labs(x="Provinces", y="Individuals", title = "Rapid Test Results")+ theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name="Result",labels=c("Negative", "Positive"))+ scale_x_discrete(breaks=c("1","2","3","4","5","7"), labels=c("ANTA", "FIAN", "TOAM", "MAHA","TOLI","ANTS"))

##Gauging percentages of each province per dependent variable##
tapply(as.numeric(dfadults$HML32)-1,dfadults$SHPROV,mean)
tapply(as.numeric(dfadults$HML35)-1,dfadults$SHPROV,mean)
tapply(as.numeric(dfkids$H22)-1,dfkids$SPROV,mean)

fever <- factor(dfkids$H22, c(0,1))
Prov <- factor(dfkids$SPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

fever <- factor(dfadults$HML32, c(0,1))
Prov <- factor(dfadults$SHPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

fever <- factor(dfadults$HML35, c(0,1))
Prov <- factor(dfadults$SHPROV, c(0,1,2,3,4,5,7))
table(fever,Prov)

##Building my Logit Regression Model##

##Kids##
glm.fitkids <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 + DeforestLag2, data = dfkids, family =binomial)

step.modelH22 <- stepAIC(glm.fitkids, direction = "both", trace = FALSE)
summary(step.modelH22)

car::vif(step.modelH22)

glm.fitkids1 <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego  + Temp2 + Precip2 + DeforestLag1, data = dfkids, family =binomial)

glm.fitkids2 <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag2, data = dfkids, family =binomial)

glm.fitkids3 <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag3, data = dfkids, family =binomial)

summary(glm.fitkids)
summary(glm.fitkids1)
summary(glm.fitkids2)
summary(glm.fitkids3)

##Cook's Distance##
plot(glm.fitkids, which = 4, id.n = 3)

model.data <- augment(glm.fitkids) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = H22), alpha = .5) + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Index", y="Standard Residual", color="Fever", title="Cook's Distance")

model.data %>%
  filter(abs(.std.resid) > 3)

##VIF##
car::vif(model_glm)

wald.test(b = coef(glm.fitkids), Sigma = vcov(glm.fitkids), Terms = 17:19) ##not working##

confint(glm.fitkids) ##CIs using log-likelihood##
confint.default(glm.fitkids) ##CIs using standard errors##

##Kids Deforestation 1##

step.model1 <- stepAIC(glm.fitkids1, direction = "both", trace = FALSE)
summary(step.model1)

exp(coef(step.model1))
exp(cbind(OR = coef(step.model1), confint(step.model1)))

logLik(step.modelH22)
logLik(step.model1)
logLik(glm.fitkids)

glm.probs1 <- predict(step.model1,type = "response")
glm.probs1[50:55]
mean(glm.probs1)

glm.pred1 <- ifelse(glm.probs1 > 0.15, 1, 0)

attach(dfkids)
table(glm.pred1,H22)

mean(glm.pred1 == H22)

##Kids Deforestation 2##
step.model2 <- stepAIC(glm.fitkids2, direction = "both", trace = FALSE)
summary(step.model2)

exp(coef(step.model2))
exp(cbind(OR = coef(step.model2), confint(step.model2)))

logLik(step.model2)
logLik(glm.fitkids)

glm.probs2 <- predict(step.model2,type = "response")
glm.probs2[50:55]
mean(glm.probs2)

glm.pred1 <- ifelse(glm.probs2 > 0.15, 1, 0)

attach(dfkids)
table(glm.pred1,H22)

mean(glm.pred1 == H22)

##Kids Deforestation 3##
step.model3 <- stepAIC(glm.fitkids3, direction = "both", trace = FALSE)
summary(step.model3)

exp(coef(step.model3))
exp(cbind(OR = coef(step.model3), confint(step.model3)))

logLik(step.model3)
logLik(glm.fitkids)

glm.probs3 <- predict(step.model3,type = "response")
glm.probs3[50:55]
mean(glm.probs3)

glm.pred3 <- ifelse(glm.probs3 > 0.15, 1, 0)

attach(dfkids)
table(glm.pred3,H22)

mean(glm.pred3 == H22)

##exp(1.032e+01)
##[1] 30333.26
##exp(0.01*1.032e+01)
##[1] 1.108713

##Starting Predictions##

train = dfkids$V007<2016
glm.fitkids <- glm(H22 ~ V025 + V113 + V128 + V129 + V460 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 +DeforestLag2 +DeforestLag3, data = dfkids, family =binomial, subset = train)

glm.probs <- predict(glm.fitkids, 
                     newdata = dfkids[!train,], 
                     type = "response")

glm.pred <- ifelse(glm.probs > 0.15, 1, 0)

H22.train = dfkids$H22[!train]
table(glm.pred, H22.train)

mean(glm.pred == H22.train)

##Did not train dataset##

##Adults##
##HML32##
glm.fitadultsa <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(glm.fitadultsa)

car::vif(glm.fitadultsa)

step.modelHML32 <- stepAIC(glm.fitadultsa, direction = "both", trace = FALSE)
summary(step.modelHML32)

car::vif(step.modelHML32)

glm.fitadults23a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial) 
##Left out Tomasina##
summary(glm.fitadults23a)

car::vif(glm.fitadults23a)

glm.fitadults1a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1, data = dfadults, family =binomial)

glm.fitadults2a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag2, data = dfadults, family =binomial)

glm.fitadults3a <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag3, data = dfadults, family =binomial)

summary(glm.fitadults1a)
summary(glm.fitadults2a)
summary(glm.fitadults3a)

logLik(glm.fitadultsa)
logLik(glm.fitadults1a)
logLik(glm.fitadults2a)
logLik(glm.fitadults3a)

##Cook's Distance##
plot(glm.fitadultsa, which = 4, id.n = 3)

model.data <- augment(glm.fitadultsa) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = HML32), alpha = .5) + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Index", y="Standard Residual", color="Rapid Tests", title="Cook's Distance")


##Deforestation 1 Lag##
step.model1a <- stepAIC(glm.fitadults1a, direction = "both", trace = FALSE)
summary(step.model1a)

logLik(step.model1a)

exp(coef(step.model1a))
exp(cbind(OR = coef(step.model1a), confint(step.model1a)))

##Deforestation 2 Lag##
step.model2a <- stepAIC(glm.fitadults2a, direction = "both", trace = FALSE)
summary(step.model2a)

car::vif(step.model2a)

logLik(step.model2a)

exp(coef(step.model2a))
exp(cbind(OR = coef(step.model2a), confint(step.model2a)))

##Deforestation 3 Lag##
step.model3a <- stepAIC(glm.fitadults3a, direction = "both", trace = FALSE)
summary(step.model3a)

logLik(step.model3a)

exp(coef(step.model3a))
exp(cbind(OR = coef(step.model3a), confint(step.model3a)))

##HML35##
glm.fitadultsb <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(glm.fitadultsb)

step.modelHML35 <- stepAIC(glm.fitadultsb, direction = "both", trace = FALSE)
summary(step.modelHML35)

car::vif(step.modelHML35)


glm.fitadults23b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(glm.fitadults23b)

car::vif(glm.fitadults23b)


glm.fitadults1b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1, data = dfadults, family =binomial)

glm.fitadults2b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag2, data = dfadults, family =binomial)

glm.fitadults3b <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag3, data = dfadults, family =binomial)

##Cook's Distance##
plot(glm.fitadultsb, which = 4, id.n = 3)

model.data <- augment(glm.fitadultsb) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = HML35), alpha = .5) + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Index", y="Standard Residual", color="Blood Smear", title="Cook's Distance")

##Deforestation 1 Lag##
step.model1b <- stepAIC(glm.fitadults1b, direction = "both", trace = FALSE)
summary(step.model1b)

logLik(step.model1b)

exp(coef(step.model1b))
exp(cbind(OR = coef(step.model1b), confint(step.model1b)))

##Deforestation 2 Lag##
step.model2b <- stepAIC(glm.fitadults2b, direction = "both", trace = FALSE)
summary(step.model2b)

logLik(step.model2b)

exp(coef(step.model2b))
exp(cbind(OR = coef(step.model2b), confint(step.model2b)))

##Deforestation 3 Lag##
step.model3b <- stepAIC(glm.fitadults3b, direction = "both", trace = FALSE)
summary(step.model3b)

logLik(step.model3b)

exp(coef(step.model3b))
exp(cbind(OR = coef(step.model3b), confint(step.model3b)))

##Log Likelihood smaller the number the better, so -3 is better than -7##

summary(glm.fitkids)
summary(glm.fitadultsa)
summary(glm.fitadultsb)

logLik(glm.fitkids)
logLik(glm.fitadultsa)
logLik(glm.fitadultsb)

##Stargazer Summary Models##

stargazer(glm.fitkids1, step.modelH22, title="Results", align=TRUE, out="models.xls")
stargazer(glm.fitadults23a, step.modelHML32, title="Results", align=TRUE, out="models1.xls")
stargazer(glm.fitadults23b, step.modelHML35, title="Results", align=TRUE, out="models2.xls")


logLik(step.model1b)
logLik(step.model2b)
logLik(step.model3b)

###Analyzing all the models###
summary(step.modelH22)
summary(glm.fitkids1)
summary(step.modelHML32)
summary(glm.fitadults23a)
summary(step.modelHML35)
summary(glm.fitadults23b)


logLik(step.modelH22)
logLik(glm.fitkids1)
logLik(step.modelHML32)
logLik(glm.fitadults23a)
logLik(step.modelHML35)
logLik(glm.fitadults23b)


exp(coef(step.modelH22)*.01)
exp(coef(glm.fitkids1)*.01)
exp(coef(step.modelHML32)*.01)
exp(coef(glm.fitadults23a)*.01)
exp(coef(step.modelHML35)*.01)
exp(coef(glm.fitadults23b)*.01)


Models
H22final <- glm(H22 ~ V128 + V129 + Temp + Precip2 + DeforestLag1 + Tana + Fianar + Mahaj + Toli + Diego, data = dfkids, family =binomial)
summary(H22final)


HML32final <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial) 
summary(HML32final)


HML35final <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(HML35final)

logLik(H22final)
logLik(HML32final)
logLik(HML35final)


exp(coef(H22final))
exp(coef(HML32final))
exp(coef(HML35final))


exp(cbind(OR = coef(H22final), confint(H22final)))
exp(cbind(OR = coef(HML32final), confint(HML32final)))
exp(cbind(OR = coef(HML35final), confint(HML35final)))











































##Previous Versions of Analysis##
##Kids Data Set##
##Splitting data Option 1## This option froze computer
split <- sample.split(dfkids$H22, SplitRatio = 0.80) 
train <- subset(dfkids, split == T) 
test <- subset(dfkids, split == F)

model_glm <- glm(H22 ~ V025 + V113 + V116 + V127 + V128 + V129 + V460 + SPROV + Temp + Precip + DeforestLag1, data = train, family='binomial') 
probability <- predict(model_glm, train, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)

summary(predicted_glm)
table(test$H22, predicted_glm)
print((237+22)/294)

summary(test$H22)

##Check Linear Assumption for Kids Dataset##

mydata <- train %>%
  dplyr::select_if(is.numeric)
mydata <- mydata[,c(3,4,7)]
predictors <- colnames(mydata)
mydata <- mydata%>%
  mutate(logit=log(probability/(1-probability))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value)) +geom_point(size=0.5, alpha= 0.5) +geom_smooth(method= "loess") +theme_bw() +facet_wrap(~predictors, scales = "free_y")

##Cook's Distance##
plot(model_glm, which = 4, id.n = 3)

model.data <- augment(model_glm) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = H22), alpha = .5) + theme_bw()

model.data %>%
  filter(abs(.std.resid) > 3)

car::vif(model_glm)

##Splitting Data Option 2##
##Kids Dataset##
glm.fitkids <- glm(H22 ~ V025 + V113 + V116 + V127 + V128 + V129 + V460 + SPROV + Temp + Precip + DeforestLag1, data = dfkids, family =binomial)
glm.fitkids <- glm(H22 ~ V025 + V113 + V116 + V127 + V128 + V129 + V460 + SPROV + Temp + Precip + DeforestLag2, data = dfkids, family =binomial)
glm.fitkids <- glm(H22 ~ V025 + V113 + V116 + V127 + V128 + V129 + V460 + SPROV + Temp + Precip + DeforestLag3, data = dfkids, family =binomial)

summary(glm.fitkids)

names(dfkids)

##Adults Dataset HML35##
glm.fitadults1 <- glm(HML35 ~ HV025 + HV201 + HV205 + HV213 + HV214 + HV215 + HV228 + HV253 + SHPROV + Temp + Precip + DeforestLag1, data = dfadults, family =binomial)

summary(glm.fitadults1)

names(dfadults)

##Adults Dataset HML32##
glm.fitadults2 <- glm(HML32 ~ HV025 + HV201 + HV205 + HV213 + HV214 + HV215 + HV228 + HV253 + SHPROV + Temp + Precip + DeforestLag1, data = dfadults, family =binomial)

summary(glm.fitadults2)

##Stepwise Fitting Models##
##Kids##

step.model <- stepAIC(glm.fitkids, direction = "both", trace = FALSE)
summary(step.model)

backwards <- step(glm.fitkids)
summary(backwards)

car::vif(backwards)

forwards <- step(model_glm, direction="forward")
summary(forwards)



model_glm2 <- glm(H22 ~ V025 + V113 + V116 + V127 + V128 + V129 + V460 + SPROV + Temp + Precip + Precip2 + DeforestLag1, data = dfkids, family='binomial') 

summary(model_glm2)
backwards2 <- step(model_glm2)
summary(backwards2)

##Forcing in water, mosquito nets, and floor type##
model_glm3 <- glm(H22 ~ V113 + V127 + V128 + V129 + V460 + SPROV + Temp + Precip2 + DeforestLag1, data = dfkids, family='binomial') 

summary(model_glm3)

##Adults Dataset##
glm.fitadults1 <- glm(HML35 ~ HV025 + HV201 + HV205 + HV213 + HV214 + HV215 + HV228 + HV253 + SHPROV + Temp + Precip + Precip2 + DeforestLag1 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)

backwards <- step(glm.fitadults2)
summary(backwards)
car::vif(backwards)


names(dfadults)

##Adults Dataset HML32##
glm.fitadults2 <- glm(HML32 ~ HV025 + HV201 + HV205 + HV213 + HV214 + HV215 + HV228 + HV253 + SHPROV + Temp + Precip + Precip2 + DeforestLag1 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)

backwards2 <- step(glm.fitadults2)
summary(backwards2)
car::vif(backwards2)
