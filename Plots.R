
#install.packages("ggplot2")
#install.packages("ggsignif")
#install.packages("ggpubr")
library(ggplot2)
library(ggsignif)
library(ggpubr)
dataset <- read.csv('Documents/Empirical_study_impl_cs22s504/dataset.csv')

#Scatter plot between RFC and number of defects
sample_data1<- data.frame(dataset$rfc,dataset$bugs)
ggplot( sample_data1, aes( x=dataset$rfc, y=dataset$bugs )) + geom_point(pch=10)+stat_cor(method = "spearman", label.x = -5, label.y = 30)

#Scatter plot between WMC and number of defects
sample_data2<- data.frame(dataset$wmc,dataset$bugs)
ggplot( sample_data2, aes( x=dataset$wmc, y=dataset$bugs )) + geom_point(pch=10)+stat_cor(method = "spearman", label.x = -5, label.y = 30)

#Scatter plot between CBO and number of defects
sample_data3<- data.frame(dataset$cbo,dataset$bugs)
ggplot( sample_data3, aes( x=dataset$cbo, y=dataset$bugs )) + geom_point(pch=10)+stat_cor(method = "spearman", label.x = -5, label.y = 30)


#Scatter plot between LCOM and number of defects
sample_data4<- data.frame(dataset$lcom,dataset$bugs)
ggplot( sample_data4, aes( x=dataset$lcom, y=dataset$bugs )) + geom_point(pch=10)+stat_cor(method = "spearman", label.x = -5, label.y = 30)


#Scatter plot between Fan-in and number of defects
sample_data5<- data.frame(dataset$fi,dataset$bugs)
ggplot( sample_data5, aes( x=dataset$fi, y=dataset$bugs )) + geom_point(pch=10)+stat_cor(method = "spearman", label.x = -5, label.y = 30)


#Scatter plot between Fan-out and number of defects
sample_data6<- data.frame(dataset$fo,dataset$bugs)
ggplot( sample_data6, aes( x=dataset$fo, y=dataset$bugs )) + geom_point(pch=10)+stat_cor(method = "spearman", label.x = -5, label.y = 30)




