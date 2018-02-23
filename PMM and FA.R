### Imputation for numeric data using PMM ###
### ------------------------------------- ###

#Loading data
missing.data <- airquality
#loading mice package
library(mice)
#analysing pattern of missing.data
md.pattern(missing.data)
#insering NA into data
missing.data[c(32,34,36),2] <- NA
missing.data[c(7:9,c(24,29,30)),3] <- NA
missing.data[1:4,4] = NA
missing.data[148:153,5] <- NA
missing.data[126:129,6] <- NA
#reanalysing the pattern of missing.data
md.pattern(missing.data)
#loading vim package for visualizing the plot
library(VIM)
aggr.data <- aggr(missing.data,col=c('lightblue','yellow'),numbers = TRUE,
                  gap = 2)

#predecting missing data
pre.miss <- mice(missing.data,m=5,maxit = 30)
summary(pre.miss)
#looking into predectied values
pre.miss$imp
#Imputing one of the five predected sets into data
total.data <- complete(pre.miss,4)
#analysing pattern of complete data
md.pattern(total.data)


### Reducing number of dimensions usinf FA for numberic data ###
### -------------------------------------------------------- ###

#loading required packages
library(psych)
library(psy)
#By using boxplot identify data is positive or negative skewed 
#and apply transformation
boxplot(total.data$Ozone, horizontal = TRUE)
boxplot(total.data$Solar.R, horizontal = TRUE)
#usinf FA method to find factors with orthogonal rotation 
fit <- factanal(total.data,factors = 3,rotation = 'varimax')
fit
#draw screeplot to identify number of factors required
scree.plot(fit$correlation)
#plotting factors together
load.data <- fit$loadings[,1:3]
plot(load.data, type ='n')
text(load.data,labels = names(total.data),cex = 0.7)

#using FA method to find factors with oblique rotation
#load below package for oblique rotation
library(GPArotation)
obl.fit <- factanal(total.data,factors = 3,rotation="oblimin")
obl.fit
scree.plot(obl.fit$correlation)
load.obl.data <- fit$loadings[,1:3]
plot(load.obl.data, type ='n')
text(load.obl.data,labels = names(total.data),cex = 0.7)

#To plot more than one factor at a time
pairs(fit$loadings)
pairs(fit$loadings, col=1:ncol(total.data), upper.panel=NULL,main="Factor loadings")
legend('topright', pch='o', col=1:ncol(total.data), 
       attr(fit$loadings, 'dimnames')[[1]], title="Variables")

#load corrplot package
library(corrplot)
corrplot(fit$loadings[,1:3])
