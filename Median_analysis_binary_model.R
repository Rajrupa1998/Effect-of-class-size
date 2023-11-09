#Median effect- Binary model

#Reading the JURE dataset
dataset <- read.csv('Documents/Empirical_study_impl_cs22s504/dataset.csv', stringsAsFactors=T)

#In binary model prone(defect proneness) is considered as dependent variable, loc(class size) is the mediator and single OO metric is considered as the independent variable.

dataset$prone <- as.factor(dataset$prone)
#Check is a column(assigned 1 value) to consider for null models in order to estimate p value 
effect_path_null_model=glm(prone ~ Check, data=dataset, family=binomial, na.action = na.exclude)



#Median effect based on single OO metric RFC


direct_effect_path1=glm(prone ~ rfc, data=dataset, family=binomial, na.action = na.exclude)
summary(direct_effect_path1)
#Estimating p value
anova(effect_path_null_model,direct_effect_path1,test='Chisq')

indirect_effect_path1=lm(loc ~ rfc, data=dataset)
summary(indirect_effect_path1)

total_effect_path1=glm(prone ~ rfc + loc, data=dataset, family=binomial, na.action = na.exclude)
summary(total_effect_path1)
#Estimating p value
anova(effect_path_null_model,total_effect_path1,test='Chisq')


#Median effect based on single OO metric WMC


direct_effect_path2=glm(prone ~ wmc, data=dataset, family=binomial)
summary(direct_effect_path2)
#Estimating p value
anova(effect_path_null_model,direct_effect_path2,test='Chisq')

indirect_effect_path2=lm(loc ~ wmc, data=dataset)
summary(indirect_effect_path2)

total_effect_path2=glm(prone ~ wmc + loc, data=dataset, family=binomial)
summary(total_effect_path2)
#Estimating p value
anova(effect_path_null_model,total_effect_path2,test='Chisq')


#Median effect based on single OO metric cbo


direct_effect_path3=glm(prone ~ cbo, data=dataset, family=binomial)
summary(direct_effect_path3)
#Estimating p value
anova(effect_path_null_model,direct_effect_path3,test='Chisq')


indirect_effect_path3=lm(loc ~ cbo, data=dataset)
summary(indirect_effect_path3)

total_effect_path3=glm(prone ~ cbo + loc, data=dataset, family=binomial)
summary(total_effect_path3)
#Estimating p value
anova(effect_path_null_model,total_effect_path3,test='Chisq')

#Median effect based on single OO metric lcom


direct_effect_path4=glm(prone ~ lcom, data=dataset, family=binomial)
summary(direct_effect_path4)
#Estimating p value
anova(effect_path_null_model,direct_effect_path4,test='Chisq')

indirect_effect_path4=lm(loc ~ lcom, data=dataset)
summary(indirect_effect_path4)

total_effect_path4=glm(prone ~ lcom + loc, data=dataset, family=binomial)
summary(total_effect_path4)
#Estimating p value
anova(effect_path_null_model,total_effect_path4,test='Chisq')



#Median effect based on single OO metric Fan-in


direct_effect_path5=glm(prone ~ fi, data=dataset, family=binomial)
summary(direct_effect_path5)
#Estimating p value
anova(effect_path_null_model,direct_effect_path5,test='Chisq')

indirect_effect_path5=lm(loc ~ fi, data=dataset)
summary(indirect_effect_path5)

total_effect_path5=glm(prone ~ fi + loc, data=dataset, family=binomial)
summary(total_effect_path5)
#Estimating p value
anova(effect_path_null_model,total_effect_path5,test='Chisq')



#Median effect based on single OO metric Fan-out


direct_effect_path6=glm(prone ~ fo, data=dataset, family=binomial)
summary(direct_effect_path6)
#Estimating p value
anova(effect_path_null_model,direct_effect_path6,test='Chisq')

indirect_effect_path6=lm(loc ~ fo, data=dataset)
summary(indirect_effect_path6)

total_effect_path6=glm(prone ~ fo + loc, data=dataset, family=binomial)
summary(total_effect_path6)
#Estimating p value
anova(effect_path_null_model,total_effect_path6,test='Chisq')


#R package of mediation(Bootstrap)
#install.packages("mediation")
set.seed(123)
library(mediation)

#R package of mediation(Bootstrap)-In applying bootstrapping mediation analysis 5000 bootstrap resamples have been employed with confidence interval as 0.95)

results_rfc= mediate(indirect_effect_path1, total_effect_path1, sims=5000, treat= 'rfc', mediator = 'loc', boot= T, conf.level = 0.95)
summary(results_rfc)

results_wmc= mediate(indirect_effect_path2, total_effect_path2, sims=5000, treat= 'wmc', mediator = 'loc', boot= T, conf.level = 0.95)
summary(results_wmc)

results_cbo= mediate(indirect_effect_path3, total_effect_path3, sims=5000, treat= 'cbo', mediator = 'loc', boot= T, conf.level = 0.95)
summary(results_cbo)


results_lcom= mediate(indirect_effect_path4, total_effect_path4, sims=5000, treat= 'lcom', mediator = 'loc', boot= T, conf.level = 0.95)
summary(results_lcom)

results_fi= mediate(indirect_effect_path5, total_effect_path5, sims=5000, treat= 'fi', mediator = 'loc', boot= T, conf.level = 0.95)
summary(results_fi)

results_fo= mediate(indirect_effect_path6, total_effect_path6, sims=5000, treat= 'fo', mediator = 'loc', boot= T, conf.level = 0.95)
summary(results_fo)