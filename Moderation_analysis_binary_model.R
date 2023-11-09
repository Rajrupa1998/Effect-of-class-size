#Moderation effect- Binary model
dataset <- read.csv('Documents/Empirical_study_impl_cs22s504/dataset.csv')

#In binary model prone(defect proneness) is considered as dependent variable, loc(class size) is the mediator and single OO metric is considered as the independent variable.

#Check is a column(assigned 1 value) to consider for null models in order to estimate p value 
dataset$prone <- as.factor(dataset$prone)
effect_path_null_model_moderation2=glm(prone ~ Check, data=dataset, family=binomial, na.action = na.exclude)

#Moderation effect based on single predictor RFC

dataset$loc <- as.numeric(dataset$loc)
dataset$rfc <- as.numeric(dataset$rfc)

interaction_term1 <- dataset$loc * dataset$rfc

moderation_effect_path_rfc = glm(prone ~ rfc + loc + interaction_term1, data=dataset, family= binomial)
summary(moderation_effect_path_rfc)
#Estimating p value
#anova(effect_path_null_model_moderation2,moderation_effect_path_rfc,test='Chisq')

#Moderation effect based on single predictor WMC

dataset$wmc <- as.numeric(dataset$wmc)

interaction_term2 <- dataset$loc * dataset$wmc

moderation_effect_path_wmc = glm(prone ~ wmc + loc + interaction_term2, data=dataset, family= binomial)
summary(moderation_effect_path_wmc)
#Estimating p value
#anova(effect_path_null_model_moderation2,moderation_effect_path_wmc,test='Chisq')


#Moderation effect based on single predictor CBO

dataset$cbo <- as.numeric(dataset$cbo)

interaction_term3 <- dataset$loc * dataset$cbo

moderation_effect_path_cbo = glm(prone ~ cbo + loc + interaction_term3, data=dataset, family= binomial)
summary(moderation_effect_path_cbo)
#Estimating p value
anova(effect_path_null_model_moderation2,moderation_effect_path_cbo,test='Chisq')


#Moderation effect based on single predictor LCOM

dataset$lcom <- as.numeric(dataset$lcom)

interaction_term4 <- dataset$loc * dataset$lcom

moderation_effect_path_lcom = glm(prone ~ lcom + loc + interaction_term4, data=dataset, family= binomial)
summary(moderation_effect_path_lcom)
#Estimating p value
anova(effect_path_null_model_moderation2,moderation_effect_path_lcom,test='Chisq')


#Moderation effect based on single predictor Fan-in

dataset$fi <- as.numeric(dataset$fi)

interaction_term5 <- dataset$loc * dataset$fi

moderation_effect_path_fi = glm(prone ~ fi + loc + interaction_term5, data=dataset, family= binomial)
summary(moderation_effect_path_fi)
#Estimating p value
anova(effect_path_null_model_moderation2,moderation_effect_path_fi,test='Chisq')

#Moderation effect based on single predictor Fan-out

dataset$fo <- as.numeric(dataset$fo)

interaction_term6 <- dataset$loc * dataset$fo

moderation_effect_path_fi = glm(prone ~ fi + loc + interaction_term6, data=dataset, family= binomial)
summary(moderation_effect_path_fi)
#Estimating p value
anova(effect_path_null_model_moderation2,moderation_effect_path_fi,test='Chisq')


#Moderation effect based on Multivariate predictor


interaction_term_multivariate <- dataset$loc * (dataset$rfc + dataset$wmc + dataset$cbo + dataset$lcom + dataset$fi + dataset$fo)

moderation_effect_path_multivariate= glm(prone ~ rfc + wmc + cbo +lcom +fi + fo + loc + interaction_term_multivariate, data=dataset, family= binomial)
summary(moderation_effect_path_multivariate)
#Estimating p value
#anova(effect_path_null_model_moderation2,moderation_effect_path_multivariate,test='Chisq')