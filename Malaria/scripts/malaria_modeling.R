# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Malaria Project Modeling #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# This script builds and evaluates models
# Final models are chosen at the end of the script


# Packages/Functions ----
source('Malaria/functions/malaria_functions.R')


##Building the Logit Model##
##For the Children Dataset##
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

glm.pred2 <- ifelse(glm.probs2 > 0.15, 1, 0)

attach(dfkids)
table(glm.pred2,H22)

mean(glm.pred2 == H22)

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

##For the Adult Dataset##
##HML32##
glm.fitadultsa <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV228 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Tomas + Temp + Temp2 + Precip + Precip2 + DeforestLag1 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(glm.fitadultsa)

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


##Final Models##
H22final <- glm(H22 ~ V128 + V129 + Temp + Precip2 + DeforestLag1 + Tana + Fianar + Mahaj + Toli + Diego, data = dfkids, family =binomial)
summary(H22final)


HML32final <- glm(HML32 ~ HV025 + HV201 + HV214 + HV215 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial) 
summary(HML32final)


HML35final <- glm(HML35 ~ HV025 + HV201 + HV214 + HV215 + HV253 + Tana + Fianar + Mahaj + Toli + Diego + Temp + Precip2 + DeforestLag2 + DeforestLag3, data = dfadults, family =binomial)
summary(HML35final)

logLik(H22final)
logLik(HML32final)
logLik(HML35final)


exp(coef(H22final)*.01)
exp(coef(HML32final)*.01)
exp(coef(HML35final)*.01)


exp(cbind(OR = (coef(H22final)*.01), confint(H22final)))
exp(cbind(OR = (coef(HML32final)*.01), confint(HML32final)))
exp(cbind(OR = (coef(HML35final)*.01), confint(HML35final)))


library(jtools)
export_summs(H22final, HML32final, HML35final, confint = FALSE, scale = TRUE)
export_summs(H22final, HML32final, HML35final, exp = TRUE, confint = FALSE, scale = TRUE)

##Predictions were not necessary for this model since the research was simply trying to see if individuals in areas with higher deforestation were more likely to have malaria##

##Dependent Variables##
#Model 1 - H22 - Fever in the last 2 weeks (1 = Yes/0 = No)
#Model 2 - HML32 - Rapid Test Results (1 = Positive/0 = Negative)
#Model 3 - HML35 - Blood Smear Results (1 = Positive/0 = Negative)

##Variable Names of Interest##
#V128 & HV214 - Wall Construction (1 = Finished/0 = Unfinished)
#V129 - Floor Construction (1 = Finished/0 = Unfinished)
#HV025 - Location (1 = Urban/0 = Rural)
#HV201 - Drinking Water Source (1 = Protected/0 = Unprotected)
#HV215 - Roof Construction (1 = Finished/0 = Unfinished)
#HV253 - House sprayed in last 12 months (1 = Yes/0 = No)



