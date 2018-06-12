#Author  : Naren Nandi
#Created : 06/12/2018

#Going to predict Infant Mortality using a Multi-Linear Model
#Using Best Subset Selection to see what Variables best fits the model.

#We will use the classic swiss data set provided with R datasets. 
#Having a look at its documentation.
data(swiss)
?swiss

#Viewing the data set in another window
View(swiss)

#Checking how these variables interact with each other, we can create a scatter plot matrix.
pairs(swiss)

#Checking the range and the structure of the data
summary(swiss)
str(swiss)

#Checking how correlated the independent variables are with the dependent variable.
#using a correlation matrix
cor(swiss)

#We can see that Infant.Mortality is positively correlated with Fertility (obviously) with 
#being Catholic and negatively with Examination and Education. Additionally we see that 
#Fertility is positively correlated with being Catholic and with Agriculture and negatively
#with Education and Examination.


#Selecting the best predictors using Best Subset Selection
#Installing the Leaps Package for this
install.packages('leaps')
library(leaps)

#utilizing the regsubsets function to get the best set of predcitors
#it does this by identifying the model with the least RSS (Residual Sum of Squares)
best.subset <- regsubsets(Infant.Mortality~., swiss, nvmax=5)
best.subset.summary <- summary(best.subset)

#The outmat field on the summary contains a matrix with the best subset of predictors 
#So choosing to view only the outmat
best.subset.summary$outmat

#The summary object also includes metrics such as adjusted R2, CP, or BIC, that we
#can use to determine the best overall model.
best.subset.by.adjr2 <- which.max(best.subset.summary$adjr2)
best.subset.by.adjr2

#Adjusted R2 tells us that the best model is that with two variables
#that is Fertility and Education.

#Checking cp
best.subset.by.cp <- which.min(best.subset.summary$cp)
best.subset.by.cp

#Using CP we arrive to the same conclusion.

#Checking bic
best.subset.by.bic <- which.min(best.subset.summary$bic)
best.subset.by.bic

#using BIC we should go for the model using just Fertility as a predictor. 

#Plotting this information and using the points function to highlight the respective best predictors 
par(mfrow=c(2,2))
plot(best.subset$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(best.subset.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(best.subset.by.adjr2, best.subset.summary$adjr2[best.subset.by.adjr2], col="red", cex =2, pch =20)
plot(best.subset.summary$cp, xlab="Number of Variables", ylab="CP", type="l")
points(best.subset.by.cp, best.subset.summary$cp[best.subset.by.cp], col="red", cex =2, pch =20)
plot(best.subset.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(best.subset.by.bic, best.subset.summary$bic[best.subset.by.bic], col="red", cex =2, pch =20)

#We can see that the 2-variable model is not that bad regarding the BIC coefficient
#Therefore we show the coefficients of the 2-variable model using Fertility and Education 
#as inputs to predict Infant.Mortality.

