#Moderation effect- Count model

#Reading the JURE dataset
dataset <- read.csv('Documents/Empirical_study_impl_cs22s504/dataset.csv')
#Check is a column(assigned 1 value) to consider for null models in order to estimate p value 
effect_path_null_model_moderation1=glm(bugs ~ Check, data=dataset, family=poisson, na.action = na.exclude)

#In binary model bugs(number of defects) is considered as dependent variable, loc(class size) is the mediator and single OO metric is considered as the independent variable.

#Moderation effect based on single predictor RFC

dataset$loc <- as.numeric(dataset$loc)
dataset$rfc <- as.numeric(dataset$rfc)

interaction_term1 <- dataset$loc * dataset$rfc

moderation_effect_path_rfc = glm(bugs ~ rfc + loc + interaction_term1, data=dataset, family= poisson, na.action = na.exclude)
summary(moderation_effect_path_rfc)
#Estimating p value
#anova(effect_path_null_model_moderation1,moderation_effect_path_rfc,test='Chisq')

#Moderation effect based on single predictor WMC

dataset$wmc <- as.numeric(dataset$wmc)

interaction_term2 <- dataset$loc * dataset$wmc

moderation_effect_path_wmc = glm(bugs ~ wmc + loc + interaction_term2, data=dataset, family= poisson)
summary(moderation_effect_path_wmc)

#Estimating p value
#anova(effect_path_null_model_moderation1,moderation_effect_path_wmc,test='Chisq')

#Moderation effect based on single predictor CBO

dataset$cbo <- as.numeric(dataset$cbo)

interaction_term3 <- dataset$loc * dataset$cbo

moderation_effect_path_cbo = glm(bugs ~ cbo + loc + interaction_term3, data=dataset, family= poisson)
summary(moderation_effect_path_cbo)

#Estimating p value
anova(effect_path_null_model_moderation1,moderation_effect_path_cbo,test='Chisq')


#Moderation effect based on single predictor LCOM

dataset$lcom <- as.numeric(dataset$lcom)

interaction_term4 <- dataset$loc * dataset$lcom

moderation_effect_path_lcom = glm(bugs ~ lcom + loc + interaction_term4, data=dataset, family= poisson)
summary(moderation_effect_path_lcom)

#Estimating p value
anova(effect_path_null_model_moderation1,moderation_effect_path_lcom,test='Chisq')


#Moderation effect based on single predictor Fan-in

dataset$fi <- as.numeric(dataset$fi)

interaction_term5 <- dataset$loc * dataset$fi

moderation_effect_path_fi = glm(bugs ~ fi + loc + interaction_term5, data=dataset, family= poisson)
summary(moderation_effect_path_fi)

#Estimating p value
anova(effect_path_null_model_moderation1,moderation_effect_path_fi,test='Chisq')


#Moderation effect based on single predictor Fan-out

dataset$fo <- as.numeric(dataset$fo)

interaction_term6 <- dataset$loc * dataset$fo

moderation_effect_path_fo = glm(bugs ~ fo + loc + interaction_term6, data=dataset, family= poisson)
summary(moderation_effect_path_fo)

#Estimating p value
anova(effect_path_null_model_moderation1,moderation_effect_path_fo,test='Chisq')


#Moderation effect based on Multivariate predictor

interaction_term_multivariate <- dataset$loc * (dataset$rfc + dataset$wmc + dataset$cbo + dataset$lcom + dataset$fi + dataset$fo)

moderation_effect_path_multivariate= glm(bugs ~ rfc + wmc + cbo +lcom +fi + fo + loc + interaction_term_multivariate, data=dataset, family= poisson)
summary(moderation_effect_path_multivariate)

#Estimating p value
#anova(effect_path_null_model,moderation_effect_path_multivariate,test='Chisq')













