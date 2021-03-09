# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Modeling #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# This script builds and evaluates models
# Final models are identified at the end of the script


## Packages/Functions ----
source('Malaria/functions/malaria_functions.R')
library(MASS)
library(broom)
library(stargazer)


## Loading Data ----
dfadults <- fread(paste0(dir$final,'dfadults.csv'))
dfkids   <- fread(paste0(dir$final,'dfkids.csv'))


## H22 (Fever in the past 2 weeks) ----
vars.22 <- colnames(dfkids[,.(V025,V113,V128,V129,V460
                           ,Tana,Fianar,Mahaj,Toli,Diego
                           ,Temp,Temp2,Precip,Precip2
                           ,DeforestLag1,DeforestLag2,DeforestLag3)])

glm.H22 <- Modeling(data=dfkids, d.var='H22', i.vars=vars.22, aic=FALSE)
step.H22 <- Modeling(data=dfkids, d.var='H22', i.vars=vars.22, aic=TRUE)

summary(glm.H22)
summary(step.H22)
car::vif(glm.H22)
car::vif(step.H22)


logLik(glm.H22)
logLik(step.H22)

# Cook's Distance
plot(glm.H22)

model.data <- augment(glm.H22) %>%
  mutate(index = 1:n())

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = H22), alpha = .5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(
  x = "Index",
  y = "Standard Residual",
  color = "Fever",
  title = "Cook's Distance"
)

# No observations have a cook's distance that 
# exceeds 3 so all observations were retained in
# this dataset

confint(glm.H22) 

# H22 Deforestation Lag 1 Only
glm1.H22 <- Modeling(data=dfkids
                      , d.var='H22'
                      , i.vars=vars.22[!vars.22 %in% c('DeforestLag2','DeforestLag3')]
                      , aic=FALSE)

step1.H22 <- Modeling(data=dfkids
                      , d.var='H22'
                      , i.vars=vars.22[!vars.22 %in% c('DeforestLag2','DeforestLag3')]
                      , aic=TRUE)
summary(glm1.H22)
summary(step1.H22)
car::vif(glm1.H22)
car::vif(step1.H22)

exp(coef(step1.H22))
exp(cbind(coef = coef(step1.H22), confint(step1.H22)))

logLik(step1.H22)

glm.probs1 <- predict(step1.H22,type = "response")
glm.probs1[50:55]
mean(glm.probs1)

glm.pred1 <- ifelse(glm.probs1 > 0.15, 1, 0)

table(glm.pred1,dfkids$H22)

mean(glm.pred1 == dfkids$H22)

# H22 Deforestation Lag 2 Only
glm2.H22 <- Modeling(data=dfkids
                      , d.var='H22'
                      , i.vars=vars.22[!vars.22 %in% c('DeforestLag1','DeforestLag3')]
                      , aic=FALSE)

step2.H22 <- Modeling(data=dfkids
                      , d.var='H22'
                      , i.vars=vars.22[!vars.22 %in% c('DeforestLag1','DeforestLag3')]
                      , aic=TRUE)
summary(glm2.H22)
summary(step2.H22)
car::vif(glm2.H22)
car::vif(step2.H22)

exp(coef(step2.H22))
exp(cbind(coef = coef(step2.H22), confint(step2.H22)))

logLik(step2.H22)

glm.probs2 <- predict(step2.H22,type = "response")
glm.probs2[50:55]
mean(glm.probs2)

glm.pred2 <- ifelse(glm.probs2 > 0.15, 1, 0)

table(glm.pred2,dfkids$H22)

mean(glm.pred2 == dfkids$H22)

# H22 Deforestation Lag 3 Only
glm3.H22 <- Modeling(data=dfkids
                      , d.var='H22'
                      , i.vars=vars.22[!vars.22 %in% c('DeforestLag1','DeforestLag2')]
                      , aic=FALSE)

step3.H22 <- Modeling(data=dfkids
                      , d.var='H22'
                      , i.vars=vars.22[!vars.22 %in% c('DeforestLag1','DeforestLag2')]
                      , aic=TRUE)
summary(glm3.H22)
summary(step3.H22)
car::vif(glm3.H22)
car::vif(step3.H22)

exp(coef(step3.H22))
exp(cbind(coef = coef(step3.H22), confint(step3.H22)))

logLik(step3.H22)

glm.probs3 <- predict(step3.H22,type = "response")
glm.probs3[50:55]
mean(glm.probs3)

glm.pred3 <- ifelse(glm.probs3 > 0.15, 1, 0)

table(glm.pred3,dfkids$H22)

mean(glm.pred3 == dfkids$H22)


## HML32 (Rapid Test) ----
vars.32 <- colnames(dfadults[,.(HV025,HV201,HV214,HV215,HV228,HV253
                           ,Tana,Fianar,Mahaj,Toli,Diego
                           ,Temp,Temp2,Precip,Precip2
                           ,DeforestLag1,DeforestLag2,DeforestLag3)])

glm.HML32 <- Modeling(data=dfadults, d.var='HML32', i.vars=vars.32, aic=FALSE)
step.HML32 <- Modeling(data=dfadults, d.var='HML32', i.vars=vars.32, aic=TRUE)

summary(glm.HML32)
summary(step.HML32)
car::vif(glm.HML32)
car::vif(step.HML32)


logLik(glm.HML32)
logLik(step.HML32)

# Cook's Distance
plot(glm.HML32)

model.data <- augment(glm.HML32) %>%
  mutate(index = 1:n())

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = HML32), alpha = .5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(
  x = "Index",
  y = "Standard Residual",
  color = "Rapid Test",
  title = "Cook's Distance"
)

# Due to the small number of observations that exceeded
# a Cook's distance of 3 and their proximity to 3,
# all observations were retained in the dataset

confint(glm.HML32)

# HML32 Deforestation Lag 1 Only
glm1.HML32 <- Modeling(data=dfadults
                     , d.var='HML32'
                     , i.vars=vars.32[!vars.32 %in% c('DeforestLag2','DeforestLag3')]
                     , aic=FALSE)

step1.HML32 <- Modeling(data=dfadults
                      , d.var='HML32'
                      , i.vars=vars.32[!vars.32 %in% c('DeforestLag2','DeforestLag3')]
                      , aic=TRUE)
summary(glm1.HML32)
summary(step1.HML32)
car::vif(glm1.HML32)
car::vif(step1.HML32)

exp(coef(step1.HML32))
exp(cbind(coef = coef(step1.HML32), confint(step1.HML32)))

logLik(step1.HML32)

glm.probs1 <- predict(step1.HML32,type = "response")
glm.probs1[50:55]
mean(glm.probs1)

glm.pred1 <- ifelse(glm.probs1 > 0.15, 1, 0)

table(glm.pred1,dfadults$HML32)

mean(glm.pred1 == dfadults$HML32)

# HML32 Deforestation Lag 2 Only
glm2.HML32 <- Modeling(data=dfadults
                     , d.var='HML32'
                     , i.vars=vars.32[!vars.32 %in% c('DeforestLag1','DeforestLag3')]
                     , aic=FALSE)

step2.HML32 <- Modeling(data=dfadults
                      , d.var='HML32'
                      , i.vars=vars.32[!vars.32 %in% c('DeforestLag1','DeforestLag3')]
                      , aic=TRUE)
summary(glm2.HML32)
summary(step2.HML32)
car::vif(glm2.HML32)
car::vif(step2.HML32)

exp(coef(step2.HML32))
exp(cbind(coef = coef(step2.HML32), confint(step2.HML32)))

logLik(step2.HML32)

glm.probs2 <- predict(step2.HML32,type = "response")
glm.probs2[50:55]
mean(glm.probs2)

glm.pred2 <- ifelse(glm.probs2 > 0.15, 1, 0)

table(glm.pred2,dfadults$HML32)

mean(glm.pred2 == dfadults$HML32)

# HML32 Deforestation Lag 3 Only
glm3.HML32 <- Modeling(data=dfadults
                     , d.var='HML32'
                     , i.vars=vars.32[!vars.32 %in% c('DeforestLag1','DeforestLag2')]
                     , aic=FALSE)

step3.HML32 <- Modeling(data=dfadults
                      , d.var='HML32'
                      , i.vars=vars.32[!vars.32 %in% c('DeforestLag1','DeforestLag2')]
                      , aic=TRUE)
summary(glm3.HML32)
summary(step3.HML32)
car::vif(glm3.HML32)
car::vif(step3.HML32)

exp(coef(step3.HML32))
exp(cbind(coef = coef(step3.HML32), confint(step3.HML32)))

logLik(step3.HML32)

glm.probs3 <- predict(step3.HML32,type = "response")
glm.probs3[50:55]
mean(glm.probs3)

glm.pred3 <- ifelse(glm.probs3 > 0.15, 1, 0)

table(glm.pred3,dfadults$HML32)

mean(glm.pred3 == dfadults$HML32)


## HML35 (Blood Smear) ----
vars.35 <- colnames(dfadults[,.(HV025,HV201,HV214,HV215,HV228,HV253
                                ,Tana,Fianar,Mahaj,Toli,Diego
                                ,Temp,Temp2,Precip,Precip2
                                ,DeforestLag1,DeforestLag2,DeforestLag3)])

glm.HML35 <- Modeling(data=dfadults, d.var='HML35', i.vars=vars.35, aic=FALSE)
step.HML35 <- Modeling(data=dfadults, d.var='HML35', i.vars=vars.35, aic=TRUE)

summary(glm.HML35)
summary(step.HML35)
car::vif(glm.HML35)
car::vif(step.HML35)


logLik(glm.HML35)
logLik(step.HML35)

# Cook's Distance
plot(glm.HML35)

model.data <- augment(glm.HML35) %>%
  mutate(index = 1:n())

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = HML35), alpha = .5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(
  x = "Index",
  y = "Standard Residual",
  color = "Blood Test",
  title = "Cook's Distance"
)

# Due to the small number of observations that exceeded
# a Cook's distance of 3 and their proximity to 3,
# all observations were retained in the dataset

confint(glm.HML35)

# HML35 Deforestation Lag 1 Only
glm1.HML35 <- Modeling(data=dfadults
                       , d.var='HML35'
                       , i.vars=vars.35[!vars.35 %in% c('DeforestLag2','DeforestLag3')]
                       , aic=FALSE)

step1.HML35 <- Modeling(data=dfadults
                        , d.var='HML35'
                        , i.vars=vars.35[!vars.35 %in% c('DeforestLag2','DeforestLag3')]
                        , aic=TRUE)
summary(glm1.HML35)
summary(step1.HML35)
car::vif(glm1.HML35)
car::vif(step1.HML35)

exp(coef(step1.HML35))
exp(cbind(coef = coef(step1.HML35), confint(step1.HML35)))

logLik(step1.HML35)

glm.probs1 <- predict(step1.HML35,type = "response")
glm.probs1[50:55]
mean(glm.probs1)

glm.pred1 <- ifelse(glm.probs1 > 0.15, 1, 0)

table(glm.pred1,dfadults$HML35)

mean(glm.pred1 == dfadults$HML35)

# HML35 Deforestation Lag 2 Only
glm2.HML35 <- Modeling(data=dfadults
                       , d.var='HML35'
                       , i.vars=vars.35[!vars.35 %in% c('DeforestLag1','DeforestLag3')]
                       , aic=FALSE)

step2.HML35 <- Modeling(data=dfadults
                        , d.var='HML35'
                        , i.vars=vars.35[!vars.35 %in% c('DeforestLag1','DeforestLag3')]
                        , aic=TRUE)
summary(glm2.HML35)
summary(step2.HML35)
car::vif(glm2.HML35)
car::vif(step2.HML35)

exp(coef(step2.HML35))
exp(cbind(coef = coef(step2.HML35), confint(step2.HML35)))

logLik(step2.HML35)

glm.probs2 <- predict(step2.HML35,type = "response")
glm.probs2[50:55]
mean(glm.probs2)

glm.pred2 <- ifelse(glm.probs2 > 0.15, 1, 0)

table(glm.pred2,dfadults$HML35)

mean(glm.pred2 == dfadults$HML35)

# HML35 Deforestation Lag 3 Only
glm3.HML35 <- Modeling(data=dfadults
                       , d.var='HML35'
                       , i.vars=vars.35[!vars.35 %in% c('DeforestLag1','DeforestLag2')]
                       , aic=FALSE)

step3.HML35 <- Modeling(data=dfadults
                        , d.var='HML35'
                        , i.vars=vars.35[!vars.35 %in% c('DeforestLag1','DeforestLag2')]
                        , aic=TRUE)
summary(glm3.HML35)
summary(step3.HML35)
car::vif(glm3.HML35)
car::vif(step3.HML35)

exp(coef(step3.HML35))
exp(cbind(coef = coef(step3.HML35), confint(step3.HML35)))

logLik(step3.HML35)

glm.probs3 <- predict(step3.HML35,type = "response")
glm.probs3[50:55]
mean(glm.probs3)

glm.pred3 <- ifelse(glm.probs3 > 0.15, 1, 0)

table(glm.pred3,dfadults$HML35)

mean(glm.pred3 == dfadults$HML35)


## Final Analysis ----
# For simplicity of the research paper, separate models
# for each deforestation lag were not considered for the 
# final models
summary(glm.H22)
summary(glm.HML32)
summary(glm.HML35)

logLik(glm.H22)
logLik(glm.HML32)
logLik(glm.HML35)

summary(step.H22)
summary(step.HML32)
summary(step.HML35)

logLik(step.H22)
logLik(step.HML32)
logLik(step.HML35)

# Clear environment
rm(list=setdiff(ls(), c('dir', 'dfkids', 'dfadults')))


## Final Models ----
# The stepwise iterations of the models were chosen with minor tweaks
# since they contained fewer variables and returned similar results.
# In addition, the variables that remained in the stepwise models have
# relatively low VIFs meaning multicollinearity was not severe.
# Many decisions that were made during this project were related
# to prior research findings from various scholars and 
# potential policy requirements and feasibility.

# H22
# DeforestLag3 was dropped from the final model as it was not significant
# and made little sense from a research standpoint
H22final <- glm(H22 ~ V128 + V129 + Temp + Precip2 + DeforestLag1 
                + Tana + Fianar + Mahaj + Toli + Diego
                , data = dfkids, family = binomial)
summary(H22final)

# HML32
# All stepwise variables were kept in the final model
HML32final <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV253 
                  + Tana + Fianar + Mahaj + Toli + Diego 
                  + Temp + Precip2 + DeforestLag2 + DeforestLag3
                  , data = dfadults, family = binomial) 
summary(HML32final)

# HML35
# Precip and HV228 were dropped from the model to keep consistency
# with the HML32 model. HV228 was also insignificant which provided 
# further reasoning for its exclusion
HML35final <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV253 
                  + Tana + Fianar + Mahaj + Toli + Diego 
                  + Temp + Precip2 + DeforestLag2 + DeforestLag3
                  , data = dfadults, family =binomial)
summary(HML35final)

# Checking Log Likelihoods and CIs
logLik(H22final)
logLik(HML32final)
logLik(HML35final)

car::vif(H22final)
car::vif(HML32final)
car::vif(HML35final)

exp(coef(H22final)*.01)
exp(coef(HML32final)*.01)
exp(coef(HML35final)*.01)

exp(cbind(coef = (coef(H22final)*.01), confint(H22final)))
exp(cbind(coef = (coef(HML32final)*.01), confint(HML32final)))
exp(cbind(coef = (coef(HML35final)*.01), confint(HML35final)))


## Compile and Save Results ----
saveRDS(H22final, file = paste0(dir$models,"H22.RDS"))
saveRDS(HML32final, file = paste0(dir$models,"HML32.RDS"))
saveRDS(HML35final, file = paste0(dir$models,"HML35.RDS"))

stargazer(H22final, HML32final, HML35final, type="text"
          ,out=paste0(dir$models,"final_models.txt")
          ,dep.var.labels=c("Fever","Rapid Test","Blood Test"))

# For this project, predictions were not necessary since the 
# research was simply trying to see if individuals in areas 
# with higher deforestation were more likely to have contacted
# malaria than those in areas with lower deforestation

## Variable Explanations ----
#Dependent Variables
#Model 1 - H22 - Fever in the last 2 weeks (1 = Yes/0 = No)
#Model 2 - HML32 - Rapid Test Results (1 = Positive/0 = Negative)
#Model 3 - HML35 - Blood Smear Results (1 = Positive/0 = Negative)

#Independent Variables of Interest
#V025 - Location (1 = Urban/0 = Rural)
#V113 - Drinking Water Source (1 = Protected/0 = Unprotected)
#V128 - Wall Construction (1 = Finished/0 = Unfinished)
#V129 - Roof Construction (1 = Finished/0 = Unfinished)
#V460 - Household uses a mosquito net (1 = Yes/0 = No)

#HV025 - Location (1 = Urban/0 = Rural)
#HV201 - Drinking Water Source (1 = Protected/0 = Unprotected)
#HV214 - Wall Construction (1 = Finished/0 = Unfinished)
#HV215 - Roof Construction (1 = Finished/0 = Unfinished)
#HV228 - Household uses a mosquito net (1 = Yes/0 = No)
#HV253 - House sprayed in last 12 months (1 = Yes/0 = No)

#DeforestLag1 - Percent of total forest deforested in prior year
#DeforestLag2 - Percent of total forest deforested 2 years prior
#DeforestLag3 - Percent of total forest deforested 3 years prior
#Precip - Amount of precipitation
#Precip2 - Quadratic of Preip
#Temp - Average Temperature
#Temp2 - Quadratic of Temp

#Tana - Province of Antananarivo in Madagascar
#Toli - Province of Toliara in Madagascar
#Tomas - Province of Tomasina in Madagascar
#Mahaj - Province of Mahajunga in Madagascar
#Fianar - Province of Fianarantsoa in Madagascar
#Diego - Province of Diego in Madagascar


# END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
