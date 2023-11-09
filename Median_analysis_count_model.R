#Median effect- Count model

#In count model bugs(number of defects) is considered as dependent variable, loc(class size) is the mediator and single OO metric is considered as the independent variable.

#Reading the JURE dataset
dataset <- read.csv('Documents/Empirical_study_impl_cs22s504/dataset.csv')
#Check is a column(assigned 1 value) to consider for null models in order to estimate p value 
effect_path_null_model=glm(bugs ~ Check, data=dataset, family=poisson, na.action = na.exclude)


#Median effect based on single OO metric RFC

direct_effect_path1=lm(bugs ~ rfc, data=dataset)
summary(direct_effect_path1)

indirect_effect_path1=lm(loc ~ rfc, data=dataset)
summary(indirect_effect_path1)

total_effect_path1=glm(bugs ~ rfc + loc, data=dataset, family= poisson)
summary(total_effect_path1)
#Estimating p value
anova(effect_path_null_model,total_effect_path1,test='Chisq')



#Median effect based on single OO metric WMC
direct_effect_path2=lm(bugs ~ wmc, data=dataset)
summary(direct_effect_path2)

indirect_effect_path2=lm(loc ~ wmc, data=dataset)
summary(indirect_effect_path2)

total_effect_path2=glm(bugs ~ wmc + loc, data=dataset, family= poisson)
summary(total_effect_path2)
#Estimating p value
anova(effect_path_null_model,total_effect_path2,test='Chisq')


#Median effect based on single OO metric CBO
direct_effect_path3=lm(bugs ~ cbo, data=dataset)
summary(direct_effect_path3)

indirect_effect_path3=lm(loc ~ cbo, data=dataset)
summary(indirect_effect_path3)

total_effect_path3=glm(bugs ~ cbo + loc, data=dataset, family= poisson)
summary(total_effect_path3)
#Estimating p value
anova(effect_path_null_model,total_effect_path3,test='Chisq')

#Median effect based on single OO metric LCOM
direct_effect_path4=lm(bugs ~ lcom, data=dataset)
summary(direct_effect_path4)

indirect_effect_path4=lm(loc ~ lcom, data=dataset)
summary(indirect_effect_path4)

total_effect_path4=glm(bugs ~ lcom + loc, data=dataset, family= poisson)
summary(total_effect_path4)
#Estimating p value
anova(effect_path_null_model,total_effect_path4,test='Chisq')



#Median effect based on single OO metric Fan-in
direct_effect_path5=lm(bugs ~ fi, data=dataset)
summary(direct_effect_path5)

indirect_effect_path5=lm(loc ~ fi, data=dataset)
summary(indirect_effect_path5)

total_effect_path5=glm(bugs ~ fi + loc, data=dataset, family= poisson)
summary(total_effect_path5)
#Estimating p value
anova(effect_path_null_model,total_effect_path5,test='Chisq')


#Median effect based on single OO metric Fan-out
direct_effect_path6=lm(bugs ~ fo, data=dataset)
summary(direct_effect_path6)

indirect_effect_path6=lm(loc ~ fo, data=dataset)
summary(indirect_effect_path6)

total_effect_path6=glm(bugs ~ fo + loc, data=dataset, family= poisson)
summary(total_effect_path6)
#Estimating p value
anova(effect_path_null_model,total_effect_path6,test='Chisq')


#R package of mediation(Bootstrap)-In applying bootstrapping mediation analysis 5000 bootstrap resamples have been employed with confidence interval as 0.95)
#install.packages("mediation")
set.seed(123)
library(mediation)

results_rfc= mediate(indirect_effect_path1, total_effect_path1, sims=5, treat= 'rfc', mediator = 'loc', boot= T, conf.level = 0.95)
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




