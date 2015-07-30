#########################
# STAT 425
# Exam 2
# Date: 11/11/11
# Author: Luis Lin
#########################

###Install Packages - needed depending on computer

install.packages("lmtest", lib="E:/R")
library("zoo",lib.loc="E:/R")
library("lmtest",lib.loc="E:/R")

install.packages("nlme", lib="E:/R")
library("nlme",lib.loc="K:/R")

install.packages("leaps",lib="E:/R")
library("leaps",lib.loc="E:/R")

install.packages("faraway", lib="E:/R")
library("faraway",lib.loc="E:/R")

install.packages("MASS", lib="E:/R")
library("MASS",lib.loc="E:/R") 

getwd()
setwd("E:/R")

##Load data

crime=read.table(file="crime.txt", header=T)
attach(crime)
names(crime)

###########################################################
#Part a: check if response needs transformation

### fit full model
g = lm(R ~ Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X, crime) 

### box-cox

boxcox(g,plotit=T) # plotit=T is the default setting

R.trans=boxcox(g, lambda=seq(-2, 2, length=400))
round(R.trans$x[R.trans$y == max(R.trans$y)],3)

tmp=R.trans$x[R.trans$y > max(R.trans$y) - qchisq(0.95, 1)/2];
CI=range(tmp) # 95% CI.
round(CI,3)
1>CI[1] & 1<CI[2] # Check contains 1 
0>CI[1] & 0<CI[2] # Check contains 0

# Box-cox suggests that a transformation is needed since 1
# is not contained in the CI interval
# Box-cox transformation suggests the best transformation is lambda=0.25
# For the sake of interpreation, choose a log transformation,
# which is also supported by box-cox since 0 is contained in the interval

## transformed response
new.R = log(R)
g = lm(new.R ~ Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X, crime) 
summary(g)

###########################################################
#Part b: Check for outliers and inuential points.

### Check for outliers.

n=nrow(crime);n                           #data points
p=length(g$coeff); p                      #predictors plus intercept
alpha=0.05
tcrit=abs(qt(alpha/(2*n),n-p-1))          #critical value

jack = rstudent(g)
names(jack)=1:n
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
abline(h=tcrit,lty=2);abline(h=-tcrit,lty=2)

identify(1:n,jack,1:n)

tcrit
outliers=jack[abs(jack)>abs(tcrit)]; outliers

###  Check for inuential points.

cook =cooks.distance(g)
names(cook)=1:n
highinf=cook[cook>=1]
highnumber=length(highinf)

plot(cook,ylab="Cooks distances")
abline(h=1,lty=2)
identify(1:n,cook,1:n)

highinf

###########################################################
#Part c: Comment on collinearity, and its effect on the variance of 
#the coefficient estimates.

#"In this situation the coefficient estimates may change erratically in response to small changes in the model or the data
#The standard errors of the affected coefficients tend to be large (leads to a failure to reject the null hypothesis that coefficeint is equal to zero)

#Possible symptoms of collinearity: high pair-wise (sample) correlation between 
#predictors, high VIF, high condition number, R2 is relatively large but 
#none of the predictor is signicant (t-test)

#One cure for collinearity is amputation —too many variables are trying to do the same job of explaining
#the response. When several variables which are highly correlated are each associated with the response, we
#have to take care that we don’t conclude that the variables we drop have nothing to do with the response

#The worst consequence of multicollinearity is that it increases the variances and standard errors of the OLS estimates.  High variances mean that the estimates are imprecise,
#and therefore not very reliable. High variances and standard errors imply low t-statistics.  Thus, multicollinearity increases the probability of 
#making a type II error of accepting the null-hypothesis when it is false, and therefore concluding that X does not effect Y when in reality it does. 
#That is, multicollinearity makes it difficult to detect an effect if one exists. " - Quote

# check pairwise correlation
round(cor(crime), dig=2)

# VIF
x = model.matrix(g)[,-1]
round(vif(x), dig=3)

# Inflation factor:  the se for the coef associated is x times 
# larger than it would have been without collinearity.
round(sqrt(vif(x)))

###########################################################
#Part d: Find the multiple linear regression model that optimizes BIC.

#Model selection: leve-wise searching algorithm with AIC, BIC

# Compute RSS using regsubsets function with the following inputs
	#Model matrix (with no intercept column)
	#Data (response)
	#Include intercept
	#Number of subsets of each size to record = 2
	#Maximum size of subsets to examine = p
	#Exhaustive Search
	#Use exhaustive search

RSSleaps=crime.subsets=regsubsets(new.R ~
Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X, nbest=2,nvmax=13,data=crime)

sumleaps=summary(RSSleaps)

# performs an exhaustive search over models, and gives back the best 2 models
# (with low RSS) of each size.

names(sumleaps) # components returned by summary(RSSleaps)

sumleaps$which # A logical matrix indicating which elements are in each model

# Create vector of model sizes 
# (include the intercept, so model size = 2 is intercept + 1 predictor)
msize=apply(sumleaps$which,1,sum);
msize=as.numeric(msize)

# Calculate BIC for all models

BIC = n*log(sumleaps$rss/n) + msize*log(n);

# Dsiplay the models with the lowest 3 values for each criterion
# order(BIC)[1:3] finds the locations of the 3 lowest values in the criterion

sumleaps$which[order(BIC)[1:3],]
BIC[order(BIC)[1:3]] 

##The best model returned by BIC
# To find model for each criteria:
	#Find the location of the minimum value of the criteria
	#Find the model in the RSS matrix corresponding to this location
	#Find the variables names corresponding to this model

#BIC
varid.BIC=sumleaps$which[order(BIC)[1],]
model.BIC=names(crime)[2:dim(crime)[2]][varid.BIC[-1]]

model.BIC

### Alternative: 

## Model selection: greedy algorithm
step(g, direction="both", k=log(n)) # BIC

###########################################################
#Part e: for BIC

#State all model assumptions, and perform appropriate diagnostics. This should
#include an analysis of the assumption of uncorrelated errors.

g = lm(new.R~Age + Ed + Ex0 + W + X,data=crime)  #Suggested BIC model
summary(g)

p=length(g$coeff); p

### Check the constant variance assumption for the errors

#plot res vs fitted
par(mfrow=c(1,2))
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals")
abline(h=0)
plot(g$fit,abs(g$res),xlab="Fitted",ylab="|Residuals|")

#regression res vs fit
summary(lm(abs(g$res) ~ g$fit))

# residuals vs predictors xi

par(mfrow=c(2,3))
plot(Age,g$res,xlab="Age",ylab="Residuals")
abline(h=0)
plot(Ed,g$res,xlab="Ed: Education",ylab="Residuals")
abline(h=0)
plot(Ex0,g$res,xlab="Ex0: 1960 Exp",ylab="Residuals")
abline(h=0)
plot(W,g$res,xlab="W: Median value of transferable goods ",ylab="Residuals")
abline(h=0)
plot(X,g$res,xlab="X: Poverty",ylab="Residuals")
abline(h=0)


### Check the normality assumption.

## Q-Q Plots
par(mfrow=c(1,2))
qqnorm (residuals (g), ylab="Residuals")
qqline (residuals (g))
qqnorm(rstudent(g),ylab="Studentized residuals")
abline(0,1)

## Shapiro test
shapiro.test (residuals (g))

### Check for large leverage points.

ginf = influence (g)
lev = ginf$hat
names(lev) = 1:n

par(mfrow=c(1,2))
halfnorm(influence(g)$hat,labs=1:n, ylab="Leverages", main="Half-norm")
plot(ginf$hat,ylab="Leverages",main="Index plot of Leverages")
abline(h=2*p/nrow(crime))
identify(1:n,lev,1:n)

lev[lev > 2*p/nrow(crime)]

### Check for outliers.

n=nrow(crime);n                           #data points
p=length(g$coeff); p                      #predictors plus intercept
alpha=0.05
tcrit=abs(qt(alpha/(2*n),n-p-1))          #critical value

jack = rstudent(g)
names(jack)=1:n
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
abline(h=tcrit,lty=2);abline(h=-tcrit,lty=2)

identify(1:n,jack,1:n)

tcrit
outliers=jack[abs(jack)>abs(tcrit)]; outliers

###  Check for inuential points.

cook =cooks.distance(g)
names(cook)=1:n
highinf=cook[cook>=1]
highnumber=length(highinf)

plot(cook,ylab="Cooks distances")
abline(h=1,lty=2)
identify(1:n,cook,1:n)

highinf


### Check the structure of the relationship between the predictors and the response.

# y vs xi

pairs(~Age + Ed + Ex0 + W + X + new.R,data=crime)

# residuals vs predictors xi
# same as part a.

par(mfrow=c(2,3))
plot(Age,g$res,xlab="Age",ylab="Residuals")
abline(h=0)
plot(Ed,g$res,xlab="Ed: Education",ylab="Residuals")
abline(h=0)
plot(Ex0,g$res,xlab="Ex0: 1960 Exp",ylab="Residuals")
abline(h=0)
plot(W,g$res,xlab="W: Median value of transferable goods ",ylab="Residuals")
abline(h=0)
plot(X,g$res,xlab="X: Poverty",ylab="Residuals")
abline(h=0)

# residual vs predicted yhat
# sames as part a.

plot(g$fit,g$res,xlab="Fitted",ylab="Residuals")
abline(h=0)

# partial regressio plots

d_Age = lm(new.R~ Ed + Ex0 + W + X)$res
m_Age = lm(Age~ Ed + Ex0 + W + X)$res
gAge=lm(d_Age~m_Age )

d_Ed = lm(new.R~ Age + Ex0 + W + X)$res
m_Ed = lm(Ed~ Age + Ex0 + W + X)$res
gEd=lm(d_Ed~m_Ed)

d_Ex0 = lm(new.R~ Age + Ed + W + X)$res
m_Ex0 = lm(Ex0~ Age + Ed + U2 + W + X)$res
gEx0=lm(d_Ex0~m_Ex0) 

d_W = lm(new.R~ Age + Ed + Ex0 + X)$res
m_W = lm(W~  Age + Ed + Ex0 + X)$res
gW=lm(d_W~m_W)

d_X = lm(new.R~ Age + Ed + Ex0 + W)$res
m_X = lm(X~  Age + Ed + Ex0 + W)$res
gX=lm(d_X~m_X)

par(mfrow=c(2,3))

plot(m_Age,d_Age,xlab="Age residuals",ylab="total residuals",
main="Partial Regression")
abline(gAge)

plot(m_Ed,d_Ed,xlab="Ed residuals",ylab="total residuals",
main="Partial Regression")
abline(gEd)

plot(m_Ex0,d_Ex0,xlab="Ex0 residuals",ylab="total residuals",
main="Partial Regression")
abline(gEx0)

plot(m_W,d_W,xlab="W residuals",ylab="total residuals",
main="Partial Regression")
abline(gW)

plot(m_X,d_X,xlab="X residuals",ylab="total residuals",
main="Partial Regression")
abline(gX)

# partial residual plots

par(mfrow=c(2,3))
prplot(g,1)
prplot(g,2)
prplot(g,3)
prplot(g,4)
prplot(g,5)

# Alternative quick plot
par(mfrow=c(2,2)); plot(g)

### Check correlated errors

cor(g$res[-1], g$res[-n])

# graphical displays for correlated errors
# plot res[i] vs res[i+1]

plot(g$res[-n], g$res[-1], xlab=expression(hat(e)[i]), ylab=expression(hat(e)[i+1])); abline(0,1)
summary(lm(g$res[-1]~g$res[-n]))

# Durbin-Watson test
dwtest(new.R~Age + Ed + Ex0 +W + X,data=crime)

###########################################################
#Part f: for BIC

#After making any adjustments to the model, if necessary, give an interpretation
#of the results, providing appropriate tables and #gures.

#No adjustments necessary. All assumptions reasonable
#Final Model:

g = lm(new.R~Age + Ed + Ex0 + W + X,data=crime)
summary(g)
round(g$coeff,3)

#Interpreation: 


###########################################################
#Part g: Repead d-f with AIC

#Part d: Find the multiple linear regression model that optimizes AIC.

g = lm(new.R ~ Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X, crime) 

#Model selection: leve-wise searching algorithm with AIC

# Compute RSS using regsubsets function with the following inputs
	#Model matrix (with no intercept column)
	#Data (response)
	#Include intercept
	#Number of subsets of each size to record = 2
	#Maximum size of subsets to examine = p
	#Exhaustive Search
	#Use exhaustive search

RSSleaps=crime.subsets=regsubsets(new.R ~
Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X, nbest=2,nvmax=13,data=crime)

sumleaps=summary(RSSleaps)

# performs an exhaustive search over models, and gives back the best 2 models
# (with low RSS) of each size.

names(sumleaps) # components returned by summary(RSSleaps)

sumleaps$which # A logical matrix indicating which elements are in each model

# Create vector of model sizes 
# (include the intercept, so model size = 2 is intercept + 1 predictor)
msize=apply(sumleaps$which,1,sum);
msize=as.numeric(msize)

# Calculate AIC for all models

AIC = n*log(sumleaps$rss/n) + 2*msize;

# Dsiplay the models with the lowest 3 values for each criterion
# order(AIC)[1:3] finds the locations of the 3 lowest values in the criterion

sumleaps$which[order(AIC)[1:3],]
AIC[order(AIC)[1:3]] 

##The best model returned by AIC
	#To find model for each criteria:
	#Find the location of the minimum value of the criteria
	#Find the model in the RSS matrix corresponding to this location
	#Find the variables names corresponding to this model

# AIC
varid.AIC=sumleaps$which[order(AIC)[1],]
model.AIC=names(crime)[2:dim(crime)[2]][varid.AIC[-1]]

# Models
model.AIC

### Alternative: 
## Model selection: greedy algorithm
step(g, direction="both")           # AIC

#Part e: for AIC

#State all model assumptions, and perform appropriate diagnostics. This should
#include an analysis of the assumption of uncorrelated errors.

g = lm(new.R~Age + Ed + Ex0 + U1 + U2 + W + X,data=crime)  #Suggested BIC model
summary(g)

p=length(g$coeff); p

### Check the constant variance assumption for the errors

#plot res vs fitted
par(mfrow=c(1,2))
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals")
abline(h=0)
plot(g$fit,abs(g$res),xlab="Fitted",ylab="|Residuals|")

#regression res vs fit
summary(lm(abs(g$res) ~ g$fit))

# residuals vs predictors xi

par(mfrow=c(2,4))
plot(Age,g$res,xlab="Age",ylab="Residuals")
abline(h=0)
plot(Ed,g$res,xlab="Ed: Education",ylab="Residuals")
abline(h=0)
plot(Ex0,g$res,xlab="Ex0: 1960 Exp",ylab="Residuals")
abline(h=0)
plot(U1,g$res,xlab="U1: Unemployment age 14-24",ylab="Residuals")
abline(h=0)
plot(U2,g$res,xlab="U2: Unemployment age 35-39",ylab="Residuals")
abline(h=0)
plot(W,g$res,xlab="W: Median value of transferable goods ",ylab="Residuals")
abline(h=0)
plot(X,g$res,xlab="X: Poverty",ylab="Residuals")
abline(h=0)


### Check the normality assumption.

## Q-Q Plots
par(mfrow=c(1,2))
qqnorm (residuals (g), ylab="Residuals")
qqline (residuals (g))
qqnorm(rstudent(g),ylab="Studentized residuals")
abline(0,1)

## Shapiro test
shapiro.test (residuals (g))

### Check for large leverage points.

ginf = influence (g)
lev = ginf$hat
names(lev) = 1:n


par(mfrow=c(1,2))
halfnorm(influence(g)$hat,labs=1:n, ylab="Leverages", main="Half-norm")
plot(ginf$hat,ylab="Leverages",main="Index plot of Leverages")
abline(h=2*p/nrow(crime))
identify(1:n,lev,1:n)

lev[lev > 2*p/nrow(crime)]

### Check for outliers.

n=nrow(crime);n                           #data points
p=length(g$coeff); p                      #predictors plus intercept
alpha=0.05
tcrit=abs(qt(alpha/(2*n),n-p-1))          #critical value

jack = rstudent(g)
names(jack)=1:n
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
abline(h=tcrit,lty=2);abline(h=-tcrit,lty=2)

identify(1:n,jack,1:n)

tcrit
outliers=jack[abs(jack)>abs(tcrit)]; outliers

###  Check for inuential points.

cook =cooks.distance(g)
names(cook)=1:n
highinf=cook[cook>=1]
highnumber=length(highinf)

par(mfrow=c(1,2))
plot(cook,ylab="Cooks distances")
abline(h=1,lty=2)
identify(1:n,cook,1:n)
halfnorm(cooks.distance(g), labs=1:n, nlab=highnumber, ylab="Cook statistics") 

highinf

### Check the structure of the relationship between the predictors and the response.

# y vs xi

pairs(new.R~Age + Ed + Ex0 + U1 + U2 + W + X,data=crime)

# residuals vs predictors xi
# same as part a.

par(mfrow=c(2,4))
plot(Age,g$res,xlab="Age",ylab="Residuals")
abline(h=0)
plot(Ed,g$res,xlab="Ed: Education",ylab="Residuals")
abline(h=0)
plot(Ex0,g$res,xlab="Ex0: 1960 Exp",ylab="Residuals")
abline(h=0)
plot(U1,g$res,xlab="U1: Unemployment age 14-24",ylab="Residuals")
abline(h=0)
plot(U2,g$res,xlab="U2: Unemployment age 35-39",ylab="Residuals")
abline(h=0)
plot(W,g$res,xlab="W: Median value of transferable goods ",ylab="Residuals")
abline(h=0)
plot(X,g$res,xlab="X: Poverty",ylab="Residuals")
abline(h=0)

# residual vs predicted yhat
# sames as part a.

plot(g$fit,g$res,xlab="Fitted",ylab="Residuals")
abline(h=0)

# partial regressio plots

d_Age = lm(new.R~ Ed + Ex0 + U1 + U2 + W + X)$res
m_Age = lm(Age~ Ed + Ex0 + U1 + U2 + W + X)$res
gAge=lm(d_Age~m_Age )

d_Ed = lm(new.R~ Age + Ex0 + U1 + U2 + W + X)$res
m_Ed = lm(Ed~ Age + Ex0 + U1 + U2  + W + X)$res
gEd=lm(d_Ed~m_Ed)

d_Ex0 = lm(new.R~ Age + Ed + U1 + U2 + W + X)$res
m_Ex0 = lm(Ex0~ Age + Ed + U1 + U2 + W + X)$res
gEx0=lm(d_Ex0~m_Ex0) 

d_U1 = lm(new.R~ Age + Ed + U2 + Ex0 + W + X)$res
m_U1 = lm(U1~  Age + Ed +U2 + Ex0 + W + X)$res
gU2=lm(d_U1~m_U2)

d_U2 = lm(new.R~ Age + Ed + U1 + Ex0 + W + X)$res
m_U2 = lm(U2~  Age + Ed + U1 + Ex0 + W + X)$res
gU2=lm(d_U2~m_U2)

d_W = lm(new.R~ Age + Ed + Ex0 + U1 + U2 + X)$res
m_W = lm(W~  Age + Ed + Ex0 + U1 + U2 + X)$res
gW=lm(d_W~m_W)

d_X = lm(new.R~ Age + Ed + Ex0 + U1 + U2 + W)$res
m_X = lm(X~  Age + Ed + Ex0 + U1 + U2 + W)$res
gX=lm(d_X~m_X)

par(mfrow=c(2,4))

plot(m_Age,d_Age,xlab="Age residuals",ylab="total residuals",
main="Partial Regression")
abline(gAge)

plot(m_Ed,d_Ed,xlab="Ed residuals",ylab="total residuals",
main="Partial Regression")
abline(gEd)

plot(m_Ex0,d_Ex0,xlab="Ex0 residuals",ylab="total residuals",
main="Partial Regression")
abline(gEx0)

plot(m_U1,d_U1,xlab="U1 residuals",ylab="total residuals",
main="Partial Regression")
abline(gU1)

plot(m_U2,d_U2,xlab="U2 residuals",ylab="total residuals",
main="Partial Regression")
abline(gU2)

plot(m_W,d_W,xlab="W residuals",ylab="total residuals",
main="Partial Regression")
abline(gW)

plot(m_X,d_X,xlab="X residuals",ylab="total residuals",
main="Partial Regression")
abline(gX)

# partial residual plots

par(mfrow=c(2,4))
prplot(g,1)
prplot(g,2)
prplot(g,3)
prplot(g,4)
prplot(g,5)
prplot(g,6)
prplot(g,7)

# Alternative quick plot
par(mfrow=c(2,2)); plot(g)

### Check correlated errors

cor(g$res[-1], g$res[-n])

# graphical displays for correlated errors
# plot res[i] vs res[i+1]

plot(g$res[-n], g$res[-1], xlab=expression(hat(e)[i]), ylab=expression(hat(e)[i+1])); abline(0,1)
summary(lm(g$res[-1]~g$res[-n]))

# Durbin-Watson test
dwtest(new.R~Age + Ed + Ex0 + U1 + U2 +W + X,data=crime)

###########################################################
#Part f: 

#After making any adjustments to the model, if necessary, give an interpretation
#of the results, providing appropriate tables and #gures.

#AIC Model: 

g = lm(new.R~Age + Ed + Ex0 + U1 + U2 + W + X,data=crime)
summary(g)

# U1 is not significant

#Adjustments necessary since assumption of linear structure between R and U1 not satisfied. 

#Correlation between U1 and U2
round(cor(crime), dig=2)[11,12]

#Look at values of AIC

sumleaps$which[order(AIC)[1:3],]
AIC[order(AIC)[1:3]] 

#AIC of model without U1 has a very close value to AIC of the current model

#Drop U1

g = lm(new.R~Age + Ed + Ex0 + U2 + W + X,data=crime)
summary(g)
round(g$coeff,3)

#Interpreation: 


#############

Analysis removing high leverage for BIC model

new.crime = crime[-29,]
attach(new.crime)

new.R = log(R)
g = lm(new.R ~ Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X, new.crime) 
summary(g)

RSSleaps=crime.subsets=regsubsets(new.R ~
Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X, nbest=2,nvmax=13,data=new.crime)

sumleaps=summary(RSSleaps)


names(sumleaps) # components returned by summary(RSSleaps)

sumleaps$which # A logical matrix indicating which elements are in each model

msize=apply(sumleaps$which,1,sum);
msize=as.numeric(msize)

BIC = n*log(sumleaps$rss/n) + msize*log(n);

sumleaps$which[order(BIC)[1:3],]
BIC[order(BIC)[1:3]] 

#BIC
varid.BIC=sumleaps$which[order(BIC)[1],]
model.BIC=names(new.crime)[2:dim(new.crime)[2]][varid.BIC[-1]]

model.BIC

g = lm(new.R~Age + Ed + Ex0 + W + X,data=new.crime)  #Suggested BIC model
summary(g)

### W no longer significant 


##############




