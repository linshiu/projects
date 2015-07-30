# Start from cleaned data

# Setup ##########################################################################

# Packages
library(car)
library(MASS)
library(corrplot)
library(BMA)

# Aginity
# main = "\\\\nas1/labuser169"
# course = "MSIA401_Project"
# setwd(file.path(main,course))

# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"
course = "MSIA_401_Statistical Methods for Data Mining"
datafolder = "Project"
setwd(file.path(main,course, datafolder))

#opts_knit$set(root.dir = getwd())

# Import data
# filename = "catalog+sales+data+for+2014+project.csv"
filename = "Catalog1.csv"
Catalog1= read.csv(filename,header = T)
Catalog1=Catalog1[,-1]

## Outliers ###################################################################

# Ridiculous yrs_since_lp
Catalog1 = Catalog1[order(Catalog1[,24], decreasing = TRUE),]
Catalog1 = Catalog1[18:101532,]

# Ridiculous yrs_since_add values
Catalog1 = Catalog1[order(Catalog1[,25], decreasing = TRUE),]
Catalog1 = Catalog1[5:101515,]

# Ridiculous ordhist1 values
Catalog1 = Catalog1[order(Catalog1[,18], decreasing = TRUE),]
Catalog1 = Catalog1[4:101511,]


## Exploratory Analysis start with raw ###############################################

filename = "catalog+sales+data+for+2014+project.csv"
rawdata = read.csv(filename,header = T)
mydata=rawdata

# create binary variable for purchase
mydata$targdol_bin = ifelse(mydata$targdol > 0,1,0)
mydata = mydata[,names(mydata)[c(18,1:17)]]  # reorder colums

# convert factor to character
# http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters

i = sapply(mydata, is.factor)
mydata[i] = lapply(mydata[i], as.character)

# date format
# http://www.statmethods.net/input/dates.html
# http://www.ats.ucla.edu/stat/r/faq/string_dates.htm
# http://stackoverflow.com/questions/9216138/find-the-day-of-a-week-in-r
# http://stackoverflow.com/questions/9749598/r-obtaining-month-and-year-from-a-date

# convert to dates
mydata[,c("datead6","datelp6")]=lapply(mydata[,c("datead6","datelp6")],
                                       function(x) as.Date(x,"%m/%d/%Y"))

class(mydata[,"datead6"]) # check

# Look at data
names(mydata)
head(mydata)
tail(mydata)
nrow(mydata)
summary(mydata)

####### Univariate stats (e.g. mean, skewness, frequency)

# http://en.wikibooks.org/wiki/R_Programming/Descriptive_Statistics
# http://www.statmethods.net/stats/descriptives.html

var_cont = c("targdol", "slstyr", "slslyr", "sls2ago" ,"sls3ago", 
             "slshist", "ordtyr", "ordlyr", "ord2ago" , "ord3ago" ,"ordhist",
             "falord", "sprord")

var_cat = c("targdol_bin" , "train","lpuryear")

var_date = c("datead6" ,"datelp6")


# install.packages("psych")
library(psych)

### Continuous Variables
ntotal = dim(mydata)[1]

stats_cont = describe(mydata[,var_cont])
n_na = ntotal-stats_cont[,"n"]
stats_cont = cbind(n=stats_cont[,"n"],n_na,stats_cont[,c("mean","sd","median","min","max",
                                                 "range","skew","kurtosis","se")])
stats_cont = round(stats_cont,2)
stats_cont

write.csv(stats_cont, "stats_cont.csv")

stats_cont_byTargdolBin = describeBy(mydata[,var_cont], group= mydata[,"targdol_bin"])
stats_cont_byTargdolBin 

### Dates
summary(mydata[,var_date])
write.csv(summary(mydata[,var_date]), "stats_cat.csv")


### Categorical

#install.packages("descr")
library("descr")
table(mydata[,var_cat])
freq(mydata[,"targdol_bin"])
freq(mydata[,"train"])
freq(mydata[,"lpuryear"])

write.csv(freq(mydata[,"targdol_bin"]), "targdol_freq.csv")
write.csv(freq(mydata[,"train"]), "train_freq.csv")
write.csv(freq(mydata[,"lpuryear"]), "lpuyear_freq.csv")

table(mydata$lpuryear,mydata$targdol_bin)

########

# look at dates
plot(table(mydata$datead6),ylab="freq",xlab="datead6")
plot(table(mydata$datelp6),ylab="freq",xlab="datelp6")

par(mfrow=c(3,3))
par(mfrow=c(1,1))

x=lapply(1:dim(mydata)[2],function(x) hist(mydata[,x],breaks=20))
print(x[[2]])

y=hist(mydata$datead6,breaks=20)
print(y)


####### Distribution histogram with boxplot

# distributions
############# plots
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
# http://seananderson.ca/2013/12/01/plyr.html
# http://stackoverflow.com/questions/16083275/histogram-with-marginal-boxplot-in-r
# http://rgraphgallery.blogspot.com/2013/04/rg-plotting-boxplot-and-histogram.html

hist()
mydata2 = subset(mydata,mydata$targdol!=0) # remove zeros
dim(mydata2)

plotHistBox = function(variable,xl,xu,title){
  nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
  par(mar=c(3.1, 3.1, 1.1, 2.1))
  hist(variable,xlim=c(xl,xu), col = "lightblue", main=title)
  boxplot(variable, horizontal=TRUE,  outline=TRUE,ylim=c(xl,xu), frame=F, col = "lightblue", width = 10)
  
  
}

# targdol
variable = mydata2$targdol
xl = 0
xu = 1600
title = "histogram of targdol"
plotHistBox(variable,xl,xu,title)

# log10(targdol+1)
variable = log10(mydata2$targdol+1)
xl = min(variable)
xu = max(variable)
title = "histogram of log(targdol+1)"
plotHistBox(variable,xl,xu,title)

###### 

# Now used the clean data with new variable for the following analysis



## Continue exploratory #########################################################
names(Catalog1)
mydata=Catalog1


# slstyr
variable = mydata$slstyr
xl = min(variable)
xu = max(variable)
title = "histogram of slstyr"
plotHistBox(variable,xl,xu,title)

# log10(slstyr+1)
variable = log(mydata$slstyr+1)
xl = min(variable)
xu = max(variable)
title = "histogram of log(slstyr+1)"
plotHistBox(variable,xl,xu,title)

# yrs_since_add
variable = mydata$yrs_since_add
xl = min(variable)
xu = max(variable)
title = "histogram of yrs_since_add"
plotHistBox(variable,xl,xu,title)

# sqrt(yrs_since_add)
variable = sqrt(mydata$yrs_since_add)
xl = min(variable)
xu = max(variable)
title = "histogram of sqrt(yrs_since_add)"
plotHistBox(variable,xl,xu,title)

# yrs_since_lp
variable = mydata$yrs_since_lp
xl = min(variable)
xu = max(variable)
title = "histogram of yrs_since_lp"
plotHistBox(variable,xl,xu,title)

# 1/yrs_since_lp
variable = 1/(mydata$yrs_since_lp+1)
xl = min(variable)
xu = max(variable)
title = "histogram of (1/yrs_since_lp+1)"
plotHistBox(variable,xl,xu,title)

# ordtyr
variable = mydata$ordtyr
xl = min(variable)
xu = max(variable)
title = "histogram of ordtyr"
plotHistBox(variable,xl,xu,title)

# 1/ordtyr
variable = 1/(mydata$ordtyr+1)
xl = min(variable)
xu = max(variable)
title = "histogram of (1/ordtyr+1)"
plotHistBox(variable,xl,xu,title)

# ordlyr
variable = mydata$ordlyr
xl = min(variable)
xu = max(variable)
title = "histogram of ordlyr"
plotHistBox(variable,xl,xu,title)

# 1/ordlyr
variable = 1/(mydata$ordlyr+1)
xl = min(variable)
xu = max(variable)
title = "histogram of (1/ordlyr+1)"
plotHistBox(variable,xl,xu,title)

# ord2ago
variable = mydata$ord2ago
xl = min(variable)
xu = max(variable)
title = "histogram of ord2ago"
plotHistBox(variable,xl,xu,title)

# 1/ord2age
variable = 1/(mydata$ord2ago+1)
xl = min(variable)
xu = max(variable)
title = "histogram of (1/ord2agor+1)"
plotHistBox(variable,xl,xu,title)

# slstyr
variable = mydata$slshist
xl = min(variable)
xu = max(variable)
title = "histogram of slshist"
plotHistBox(variable,xl,xu,title)

# log10(slshist+1)
variable = log(mydata$slshist+1)
xl = min(variable)
xu = max(variable)
title = "histogram of log(slshist+1)"
plotHistBox(variable,xl,xu,title)

# falord
variable = mydata$falord
xl = min(variable)
xu = max(variable)
title = "histogram of falord"
plotHistBox(variable,xl,xu,title)

# sqrt(falord)
variable = sqrt(mydata$falord)
xl = min(variable)
xu = max(variable)
title = "histogram of sqrt(falord)"
plotHistBox(variable,xl,xu,title)

# sprord
variable = mydata$sprord
xl = min(variable)
xu = max(variable)
title = "histogram of sprord"
plotHistBox(variable,xl,xu,title)

# sqrt(sprord)
variable = sqrt(mydata$sprord)
xl = min(variable)
xu = max(variable)
title = "histogram of sqrt(sprord)"
plotHistBox(variable,xl,xu,title)


############## Correlation plot + matrix

numeric_var = c("targdol", "slslyr", "sls2ago", "sls3ago", "slshist", "ordtyr", "ordlyr",
                "ord2ago", "ord3ago", "falord", "sprord", "ordhist1", "yrs_since_lp",
                "yrs_since_add")
corr = round(cor(mydata[,numeric_var]),2)


library(corrplot)
corrplot.mixed(corr, upper = "ellipse", lower = "number")
# corrplot function changes margin, so next graph looks messed up. change to default
par(mar=c(5,4,4,2)+0.1)


pairs(corr)

############## targdol vs predictors

mydata2 = subset(mydata,mydata$targdol!=0) # remove zeros

plot(1/mydata2$yrs_since_lp,log(mydata2$targdol+1))


plot(mydata$ordhist1,mydata$sprord,col=mydata$color)

plot(mydata$slstyr,mydata$slslyr,col=mydata$color)

plot(mydata$ordtyr,mydata$ordlyr,col=mydata$color)
legend("topright", inset = 0.05, c("1", "0"), col = c("Blue", "Red"), 
       pch= 1)

plot(log10(mydata$slshist+1),log10(mydata$targdol+1),col=mydata$color)
plot(log10(mydata$slstyr+1),log10(mydata$targdol+1),col=mydata$color)


plot(mydata$yrs_since_lp,mydata$targdol,col=mydata$color)

names(mydata)
mydata2



############## boxplot predictors by targdol_bin 

mydata$targdol_bin =as.factor(mydata$targdol_bin)
mydata$color[mydata$targdol_bin==0]="red"
mydata$color[mydata$targdol_bin==1]="blue"

ggplot(mydata, aes(x=targdol_bin, y=slstyr, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=slslyr, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=sls2ago, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=sls3ago, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)

ggplot(mydata, aes(x=targdol_bin, y=ordtyr, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=ordlyr, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=ord2ago, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=ord3ago, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)

ggplot(mydata, aes(x=targdol_bin, y=falord, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=sprord, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)

ggplot(mydata, aes(x=targdol_bin, y=ordhist1, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=slshist, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)

ggplot(mydata, aes(x=targdol_bin, y=yrs_since_lp, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata, aes(x=targdol_bin, y=yrs_since_add, fill=targdol_bin)) + geom_boxplot() + guides(fill=FALSE)


names(mydata)


# Create Training and test log and lm ###############################################
Catalog.train = Catalog1[Catalog1$train==1,]
Catalog.test = Catalog1[Catalog1$train==0,]

keepColumns = c("slstyr", "slslyr", "sls2ago", "sls3ago", "slshist", "ordtyr", "ordlyr",
                "ord2ago", "ord3ago", "falord", "sprord", "ordhist1", "falord_bin", 
                "ordtyr_bin", "ordlyr_bin" , "ord2ago_bin", "ord3ago_bin", "yrs_since_lp",
                "yrs_since_add", "targdol_bin"   )

Catalog.log.train =Catalog.train[keepColumns]
Catalog.log.test = Catalog.test[keepColumns]

keepColumns2 = c("slstyr", "slslyr", "sls2ago", "sls3ago", "slshist", "ordtyr", "ordlyr",
                 "ord2ago", "ord3ago", "falord", "sprord", "ordhist1", "falord_bin", 
                 "ordtyr_bin", "ordlyr_bin" , "ord2ago_bin", "ord3ago_bin", "yrs_since_lp",
                 "yrs_since_add", "targdol_pur"   )

Catalog.lm.train = Catalog.train[keepColumns2]
Catalog.lm.train = Catalog.lm.train[!is.na(Catalog.lm.train$targdol_pur),]

Catalog.lm.test = Catalog.test[keepColumns2]

# don't remove people with targdol = 0
# Catalog.lm.test = Catalog.lm.test[!is.na(Catalog.lm.test$targdol_pur),]

# LM Model selection ##############################################################

f1 = log10(targdol_pur+1) ~ sqrt(yrs_since_add)+sqrt(1/(yrs_since_lp+1))+sqrt(ordtyr) +
  sqrt(ordlyr)+sqrt(ord2ago)+sqrt(ord3ago)+sqrt(falord)+sqrt(sprord) +
  sqrt(ordhist1)+falord_bin+ordtyr_bin+ordlyr_bin+ord2ago_bin +
  ord3ago_bin+log10(slshist+1)+log10(slstyr+1)+log10(slslyr+1)+
  log10(sls2ago+1)+log10(sls3ago+1)+sqrt(1/(ordtyr*ordlyr+1))+
  sqrt(1/(ordtyr*ordlyr*ord2ago+1))+log10(1/(slstyr*slslyr+1))+
  log10(1/(slstyr*slslyr*sls2ago+1))+I(falord/ordhist1)+I(sqrt(ordhist1/(yrs_since_add+1)))

fit.lm1 = lm(f1, data = Catalog.lm.train)

summary(fit.lm1)
vif(fit.lm1)
# Adj.R2 = 0.1386
# Lots of insignificant coefficients; obvious multicollinearity issues

fit.lm2 = stepAIC(fit.lm1, direction = "both")

fit.lm2 = lm(formula = targdol_pur ~ sqrt(ordlyr) + sqrt(ord2ago) + sqrt(ord3ago) + 
               sqrt(falord) + sqrt(sprord) + sqrt(ordhist1) + falord_bin + 
               ordtyr_bin + ordlyr_bin + ord2ago_bin + ord3ago_bin + log10(slshist + 1) + 
               log10(slstyr + 1) + log10(slslyr + 1) + log10(sls2ago + 1) + 
               log10(sls3ago + 1) + log10(1/(slstyr * slslyr + 1)) + 
               log10(1/(slstyr * slslyr * sls2ago + 1)) + 
               I(sqrt(ordhist1/(yrs_since_add + 1))), data = Catalog.lm.train)

summary(fit.lm2)
vif(fit.lm2)
# Adj.R2 = 0.09036
# Still have insignificant predictors and plenty of multicollinearity
AIC(fit.lm2)
# 51910.31

# Starting over w/o sqrt(ord)'s and w/ more interactions
f3 = log10(targdol_pur+1) ~ sqrt(falord) + sqrt(sprord) + 
  falord_bin + ordtyr_bin + ordlyr_bin + ord2ago_bin + ord3ago_bin + 
  log10(slshist + 1) + log10(slstyr + 1) + log10(slslyr + 1) + 
  log10(sls2ago + 1) + log10(sls3ago + 1) + log10(slstyr * slslyr + 1) + 
  log10(slstyr * slslyr * sls2ago + 1) + log10(slslyr*sls2ago+1) +
  log10(slslyr*sls2ago*sls3ago+1) + log10(sls2ago*sls3ago+1) +
  I(log10(slshist+1)/sqrt(yrs_since_add + 1)) +
  I(log10(slshist+1)/sqrt(yrs_since_lp+1))

fit.lm3 = lm(formula = f3, data = Catalog.lm.train)

summary(fit.lm3)
vif(fit.lm3)

# Adj.R2 = 0.1323

n = dim(Catalog.lm.train)[1]

fit.lm4 = step(fit.lm3, k = log(n), direction = "both")

summary(fit.lm4)
# Adj.R2 = 0.1316 (Best Model)
AIC(fit.lm4)
# 2641.076

write.csv(summary(fit.lm4)$coeff,"fit_lm4.csv")
summary(fit.lm4)

vif(fit.lm4)

fit.lm5 = lm(formula = log10(targdol_pur+1) ~ sqrt(falord) + sqrt(sprord) + 
               falord_bin + ordtyr_bin + ordlyr_bin + ord2ago_bin + ord3ago_bin + 
               log10(slshist + 1) + log10(slstyr * slslyr + 1) + 
               log10(slstyr * slslyr * sls2ago + 1) + log10(slslyr*sls2ago+1) +
               log10(slslyr*sls2ago*sls3ago+1) + log10(sls2ago*sls3ago+1) +
               I(log10(slshist+1)/sqrt(yrs_since_add + 1)) +
               I(log10(slshist+1)/sqrt(yrs_since_lp+1)), data = Catalog.lm.train)

summary(fit.lm5)
# Adj.R2 = 0.101

vif(fit.lm5)

fit.lm6 = step(fit.lm5, k = log(n), direction = "both")

summary(fit.lm6)
# Adj.R2 = 0.09956 (Alternative Model)
write.csv(summary(fit.lm6)$coeff,"fit_lm6.csv")


vif(fit.lm6)

# # step using AIC (both LR and LM) ####
# library(MASS)
# fit_stepAIC = step(object = full, direction = "both")  # AIC
# 
# # step using BIC (both LR and LM) ####
# fit_stepAIC = step(object = full, direction = "both", k=log(n))  # BIC
# 
# # use single drop/add (both LR and LM) ####
# drop1(fit, test="F",data=mydata) # LRT on individual terms
# add1(fit, scope= ~ X1 + X2, test="F",data=mydata) # LRT on individual terms

# use regsubsets (LM only) ####
source("ModelSelection.R")
lm_select1 = regsubsetsF2(f3,Catalog.lm.train)
lm_select1$subsets

write.csv(lm_select1$subsets, "subsets_lm.csv")

# use glmulti (both LM and LR) ####
source("ModelSelection.R")
lm_select2 = glmulti_gaussian(f3,Catalog.lm.train)
lm_select2

# use bic.glm (both LR and LM) ####
# http://www.medicine.mcgill.ca/epidemiology/joseph/courses/EPIB-621/logselect.pdf
# Bayesian Model Averaging accounts for the model uncertainty inherent in the variable 
# selection problem by averaging over the best models in the model class according 
# to approximate posterior model probability.
# install.packages("BMA")
library(BMA)
output=bic.glm(f1,data=Catalog.lm.train,glm.family=gaussian)
summary(output)
names(output)

output$label # variables in the model
output$postprob # best model = highest posterior probability
output$probne0 # probability variable should be in the model
write.csv(output$probne0, "probInModel_lm.csv")


# LM Model diagnostics ##############################################################

## check the fit (check linearity assumption by plotting residuals against each predictor)
mydata = Catalog.lm.train
fit = fit.lm4

m = model.matrix(fit)
mnames = colnames(model.matrix(fit))
mlength = length(mnames)                               

for (i in 2:mlength ){
  plot(m[,i],fit$resid,xlab=mnames[i],ylab="residuals")   
}

# All plots look random so assumptions about the form of the model 
# (linear in the regression parameters) is satisfied. A transformation or a higher degree term in xj is
# not needed since the plots are random

## check normality (using qq plot)
qqPlot(fit, main = "Normal Q-Q Plot")

# The plot shows that most points fall along the line, indicating the normality
# assumption of errors is satisfied

## check Checking Homoscedasticity (using residuals vs. fitted)

plot(fit,which=c(1),ask=FALSE)

# The plot shows data points are random forming a parallel band, indicating the 
# common variance assumption of errors is valid (Homoscedasticity)

## check independence (not relevant since no time series data)

## Check multicollinearity

corr = round(cor(model.matrix(fit)[,-1]),2)
corr

library(car)
vif(fit)
write.csv(vif(fit), "vif_lm4.csv")

names(vif(fit))[vif(fit)>10]

library(corrplot)
corrplot.mixed(corr, upper = "ellipse", lower = "number")
# corrplot function changes margin, so next graph looks messed up. change to default
par(mar=c(5,4,4,2)+0.1)

# The scatter plots and correlation coefficients show strong correlation among some 
# predictors. In addition, the VIF > 10 for some, so the assumption of
# linearly independence of each predictor might be violated; indicating a 
# multicollinearity problem. 

# However, the VIF's are not extremely large and only a few are > 0, so there is not 
# severe multicollinearity. In this case, we can say that high VIF is not a 
# problem and can be safely ignored for two reasons.
# First, is that the high VIFs are caused by the includsion
# of interactions (including producs of other variables means that the variables
# might be highly correlated with their product). Second, the variables with VIF > 0
# are a result of dummy variables (e.g. ordtyr_bin) which are highly correlated
# with the correpsonding sales in that year (e.g. salstyr). In this problem,
# we are not very interested in determining the precise effect of each predictor,
# but rather have a model with better prediction. So this is the preferred model
# since it is the simplest that is close to the highest adjusted R^2 in the subset. 

# http://www.statisticalhorizons.com/multicollinearity

## Check influential points
# http://www.ats.ucla.edu/stat/stata/dae/rreg.htm

par(mfrow=c(2,2))
plot(fit,which=c(3),ask=FALSE) # standardized residuals vs fitted
plot(fit,which=c(4),ask=FALSE) # cook's D
plot(fit,which=c(5),ask=FALSE) # cook's D
plot(fit,which=c(6),ask=FALSE) # cook's D
par(mfrow=c(1,1))

# Compute leverage cutoff
p=dim(model.matrix(fit))[2]-1
n=dim(mydata)[1]
Lcutoff = 2*(p+1)/n
Lcutoff

# Cook cutoff
Dcutoff = 1

# influence plot
library(car)   # needed for "influencePlot" function below

influencePlot(fit) 
influenceIndexPlot(fit, id.n=3)

# Can also get studeres, hat and cook D :
influences = influence.measures(fit)
names(influences)
colnames(influences$infmat)

leverage = sort(influences$infmat[,"hat"],decreasing=TRUE)
head(leverage)

cook = sort(influences$infmat[,"cook.d"],decreasing=TRUE)
head(cook)

id_leverage = as.numeric(names(leverage[leverage>Lcutoff]))
id_cook = as.numeric(names(cook[cook>Dcutoff]))
length(id_leverage)
length(id_cook)

# Find outliers points
outliers=outlierTest(fit, cutoff=0.05) # report all observation win Beonferonni p-values < cutoff
outliers
names(outliers)
id_outlier = as.numeric(names(outliers$rstudent))

id_influential = unique(c(id_outlier,id_leverage,id_cook))
length(id_influential)
  
# This function creates a "bubble" plot of Studentized residuals by hat values, 
# with the areas of the circles representing the observations proportional to Cook's 
# distances. Vertical reference lines are drawn at twice and three times the 
# average hat value, horizontal reference lines at -2, 0, and 2 on the 
# Studentized-residual scale. (R Documentation)

# # All Cook D's are < 1, so there are no influential points when using the 
# Cutoff of D > 1 (rule of thumb) 

# There might be a couple of potential outliers since the studentized residuals
# are greater than |2|. However, conducting a statistical test only shows that
# observation 41618 is an outlier.

# Using the rule of thumb (hii>2(p+1)/2), 
# there are a few observations with high leverage (i.e. extreme value on a predictor
# variable). However,  the number of high 
# leverage points is small compare to the size of the training set, and because
# the binary predictor variables are unbalanced and can potentially create high leverage
# points.

# The identified high leverage and outliers points do not seem to be influential 
# (i.e. substantially change the coefficient estimates) according to Cook's D, 
# which combines the information of leverage and outlierness of the residual. 
# Therefore, no points were removed from the dataset. 

# http://people.stern.nyu.edu/gsimon/Pamphlets/MultipleRegressionDiagnosticsCOLLECTION.pdf

# note that rownames is different than row number
id_row = as.numeric(rownames(Catalog.lm.train))
Catalog.lm.train2 = Catalog.lm.train[!(id_row %in% id_influential),]
dim(Catalog.lm.train2)[1]-dim(Catalog.lm.train)[1]

######## (not for the report)
# refit after removing influential points (results adjR^2, VIF, etc get worse)
fit.lm4.2 = lm(formula(fit.lm4),data= Catalog.lm.train2)
summary(fit.lm4.2)
summary(fit.lm4)

mydata = Catalog.lm.train2
fit = fit.lm4.2

qqPlot(fit, main = "Normal Q-Q Plot")
plot(fit,which=c(1),ask=FALSE)
vif(fit)
names(vif(fit))[vif(fit)>10]
par(mfrow=c(2,2))
plot(fit,which=c(3),ask=FALSE) # standardized residuals vs fitted
plot(fit,which=c(4),ask=FALSE) # cook's D
plot(fit,which=c(5),ask=FALSE) # cook's D
plot(fit,which=c(6),ask=FALSE) # cook's D
par(mfrow=c(1,1))
influencePlot(fit)
influenceIndexPlot(fit, id.n=3)
outliers=outlierTest(fit, cutoff=0.05) # report all observation win Beonferonni p-values < cutoff
outliers

#########

# repeat for alternative model

## check the fit (check linearity assumption by plotting residuals against each predictor)
mydata = Catalog.lm.train
fit = fit.lm6

m = model.matrix(fit)
mnames = colnames(model.matrix(fit))
mlength = length(mnames)                               

for (i in 2:mlength ){
  plot(m[,i],fit$resid,xlab=mnames[i],ylab="residuals")   
}

# All plots look random so assumptions about the form of the model 
# (linear in the regression parameters) is satisfied. A transformation or a higher degree term in xj is
# not needed since the plots are random

## check normality (using qq plot)
qqPlot(fit, main = "Normal Q-Q Plot")

# The plot shows that most points fall along the line, indicating the normality
# assumption of errors is satisfied

## check Checking Homoscedasticity (using residuals vs. fitted)

plot(fit,which=c(1),ask=FALSE)

# The plot shows data points are random forming a parallel band, indicating the 
# common variance assumption of errors is valid (Homoscedasticity)

## check independence (not relevant since no time series data)

## Check multicollinearity

corr = round(cor(model.matrix(fit)[,-1]),2)
corr

library(car)
vif(fit)
write.csv(vif(fit), "vif_lm6.csv")

names(vif(fit))[vif(fit)>10]

library(corrplot)
corrplot.mixed(corr, upper = "ellipse", lower = "number")
# corrplot function changes margin, so next graph looks messed up. change to default
par(mar=c(5,4,4,2)+0.1)

# The scatter plots and correlation coefficients do not show strong correlation among some 
# predictors. In addition, the VIF < 10 for all predictors, so the assumption of
# linearly independence of each predictor is not  violated, indicating no
# multicollinearity problems. 

# http://www.statisticalhorizons.com/multicollinearity

## Check influential points
# http://www.ats.ucla.edu/stat/stata/dae/rreg.htm

par(mfrow=c(2,2))
plot(fit,which=c(3),ask=FALSE) # standardized residuals vs fitted
plot(fit,which=c(4),ask=FALSE) # cook's D
plot(fit,which=c(5),ask=FALSE) # cook's D
plot(fit,which=c(6),ask=FALSE) # cook's D
par(mfrow=c(1,1))

# Compute leverage cutoff
p=dim(model.matrix(fit))[2]-1
n=dim(mydata)[1]
Lcutoff = 2*(p+1)/n
Lcutoff

# Cook cutoff
Dcutoff = 1

# influence plot
library(car)   # needed for "influencePlot" function below
influencePlot(fit) 
influenceIndexPlot(fit, id.n=3)

# Can also get studeres, hat and cook D :
influences = influence.measures(fit)
names(influences)
colnames(influences$infmat)

leverage = sort(influences$infmat[,"hat"],decreasing=TRUE)
head(leverage)

cook = sort(influences$infmat[,"cook.d"],decreasing=TRUE)
head(cook)

id_leverage = as.numeric(names(leverage[leverage>Lcutoff]))
id_cook = as.numeric(names(cook[cook>Dcutoff]))
length(id_leverage)
length(id_cook)

# Find outliers points
outliers=outlierTest(fit, cutoff=0.05) # report all observation win Beonferonni p-values < cutoff
outliers
names(outliers)
id_outlier = as.numeric(names(outliers$rstudent))

id_influential = unique(c(id_outlier,id_leverage,id_cook))
length(id_influential)

# This function creates a "bubble" plot of Studentized residuals by hat values, 
# with the areas of the circles representing the observations proportional to Cook's 
# distances. Vertical reference lines are drawn at twice and three times the 
# average hat value, horizontal reference lines at -2, 0, and 2 on the 
# Studentized-residual scale. (R Documentation)

# # All Cook D's are < 1, so there are no influential points when using the 
# Cutoff of D > 1 (rule of thumb) 

# There might be a couple of potential outliers since the studentized residuals
# are greater than |2|. However, conducting a statistical test only shows that
# observation 41618 is an outlier.

# Using the rule of thumb (hii>2(p+1)/2), 
# there are a few observations with high leverage (i.e. extreme value on a predictor
# variable). However,  the number of high 
# leverage points is small compare to the size of the training set, and because
# the binary predictor variables are unbalanced and can potentially create high leverage
# points.

# The identified high leverage and outliers points do not seem to be influential 
# (i.e. substantially change the coefficient estimates) according to Cook's D, 
# which combines the information of leverage and outlierness of the residual. 
# Therefore, no points were removed from the dataset. 

# http://people.stern.nyu.edu/gsimon/Pamphlets/MultipleRegressionDiagnosticsCOLLECTION.pdf

# note that rownames is different than row number
id_row = as.numeric(rownames(Catalog.lm.train))
Catalog.lm.train2 = Catalog.lm.train[!(id_row %in% id_influential),]
dim(Catalog.lm.train2)[1]-dim(Catalog.lm.train)[1]


# LR Model selection ##############################################################


fit.log1 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + I(1/(yrs_since_lp+1)) + 
                 I(1/(ordtyr+1)) + I(1/(ordlyr+1)) + I(1/(ord2ago+1)) +
                 I(1/(ord3ago+1)) + falord_bin + ordtyr_bin + ordlyr_bin + ord2ago_bin +
                 ord3ago_bin + log10(slstyr+1) + log10(slslyr+1) + log10(sls2ago+1) + 
                 log10(sls3ago+1) + I(1/(ordlyr*ord2ago + 1)) + I(1/(ord2ago*ord3ago + 1)) +
                 I(1/(ordtyr*ordlyr*ord2ago*ord3ago + 1)) + I(falord/(ordhist1+1)) +
                 I(1/(ordhist1+1)) +
                 log10(slshist + 1) + I(1/(ordtyr * ordlyr + 1)) + 
                 I(1/(ordtyr*ordlyr*ord2ago + 1)) +
                 I(1/(ordlyr*ord2ago*ord3ago + 1)) + sqrt(ordhist1*sprord) +
                 sqrt(falord_bin/(yrs_since_lp+1)) +
                 sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log1)

# This will take some time
fit.log2 = step(fit.log1, k = log(n), direction = "both")

fit.log2 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + I(1/(yrs_since_lp + 1)) + 
                 I(1/(ordtyr + 1)) + I(1/(ordlyr + 1)) + I(1/(ord2ago + 1)) + 
                 I(1/(ord3ago + 1)) + falord_bin + ordtyr_bin + log10(slstyr + 1) + 
                 log10(slslyr + 1) + log10(sls3ago + 1) + 
                 I(1/(ordtyr * ordlyr * ord2ago * ord3ago + 1)) + 
                 log10(slshist + 1) + I(1/(ordtyr * ordlyr + 1)) + 
                 sqrt(ordhist1 * sprord) + sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log2)
# AIC = 24225 (16 predictors)

vif(fit.log2)
# Still have some big VIF issues

# Removing ordtyr_bin (due to high VIF)
fit.log3 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + I(1/(yrs_since_lp + 1)) + 
                 I(1/(ordtyr + 1)) + I(1/(ordlyr + 1)) + I(1/(ord2ago + 1)) + 
                 I(1/(ord3ago + 1)) + falord_bin + log10(slstyr + 1) + 
                 log10(slslyr + 1) + log10(sls3ago + 1) + 
                 I(1/(ordtyr * ordlyr * ord2ago * ord3ago + 1)) + 
                 log10(slshist + 1) + I(1/(ordtyr * ordlyr + 1)) + 
                 sqrt(ordhist1 * sprord) + sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log3)
# AIC = 24374 (15 predictors)

vif(fit.log3)
# Still have some VIF issues

# Removing slstyr, slslyr, and sls3ago (due to VIF)
fit.log4 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + I(1/(yrs_since_lp + 1)) + 
                 I(1/(ordtyr + 1)) + I(1/(ordlyr + 1)) + I(1/(ord2ago + 1)) + 
                 I(1/(ord3ago + 1)) + falord_bin + 
                 I(1/(ordtyr * ordlyr * ord2ago * ord3ago + 1)) + 
                 log10(slshist + 1) + I(1/(ordtyr * ordlyr + 1)) + 
                 sqrt(ordshist1 * sprord) + sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log4)
# AIC = 24398 (12 predictors)

vif(fit.log4)
# ordtyr's VIF = 11.167369

# Removing ord3ago (due to relatively low significance)
fit.log5 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + I(1/(yrs_since_lp + 1)) + 
                 I(1/(ordtyr + 1)) + I(1/(ordlyr + 1)) + I(1/(ord2ago + 1)) + 
                 falord_bin + 
                 I(1/(ordtyr * ordlyr * ord2ago * ord3ago + 1)) + 
                 log10(slshist + 1) + I(1/(ordtyr * ordlyr + 1)) + 
                 sqrt(ordhist1 * sprord) + sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log5)
# AIC = 24402 (11 predictors)

vif(fit.log5)
# ordtyr's VIF = 10.743075 (good enough I think)


fit.log6 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + sqrt(yrs_since_lp) + 
                 sqrt(ordtyr) + sqrt(ordlyr) + sqrt(ord2ago) +
                 sqrt(ord3ago) + falord_bin + ordtyr_bin + ordlyr_bin + ord2ago_bin +
                 ord3ago_bin + log10(slstyr+1) + log10(slslyr+1) + log10(sls2ago+1) + 
                 log10(sls3ago+1) + sqrt(ordlyr*ord2ago) + sqrt(ord2ago*ord3ago) +
                 sqrt(ordtyr*ordlyr*ord2ago*ord3ago) + I(falord/(ordhist1+1)) +
                 sqrt(ordhist1) +
                 log10(slshist + 1) + sqrt(ordtyr * ordlyr) + 
                 sqrt(ordtyr*ordlyr*ord2ago) +
                 sqrt(ordlyr*ord2ago*ord3ago) + sqrt(ordhist1*sprord) +
                 sqrt(falord_bin/(yrs_since_lp+1)) +
                 sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log6)

fit.log7 = step(fit.log6, k = log(n), direction = "both")

fit.log7 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + sqrt(yrs_since_lp) + 
                 sqrt(ordtyr) + sqrt(ordlyr) + sqrt(ord2ago) + sqrt(ord3ago) + 
                 falord_bin + ordtyr_bin + log10(slstyr + 1) + log10(slslyr + 1) + 
                 log10(sls3ago + 1) + sqrt(ordlyr * ord2ago) + sqrt(ord2ago * ord3ago) + 
                 sqrt(ordtyr * ordlyr * ord2ago * ord3ago) + log10(slshist + 1) + 
                 sqrt(ordtyr * ordlyr) + sqrt(ordhist1 * sprord) + 
                 sqrt(falord_bin/(yrs_since_lp + 1)) + sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log7)
# AIC = 24681 (19 predictors)

vif(fit.log7)
# Pretty big VIF issues again

# Removing ordtyr_bin (due to VIF issues)
fit.log8 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + sqrt(yrs_since_lp) + 
                 sqrt(ordtyr) + sqrt(ordlyr) + sqrt(ord2ago) + sqrt(ord3ago) + 
                 falord_bin + log10(slstyr + 1) + log10(slslyr + 1) + 
                 log10(sls3ago + 1) + sqrt(ordlyr * ord2ago) + sqrt(ord2ago * ord3ago) + 
                 sqrt(ordtyr * ordlyr * ord2ago * ord3ago) + log10(slshist + 1) + 
                 sqrt(ordtyr * ordlyr) + sqrt(ordhist1 * sprord) + 
                 sqrt(falord_bin/(yrs_since_lp + 1)) + sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log8)
# AIC = 24773 (18 predictors)

vif(fit.log8)
# Still have a bunch of issues

# Removing slstyr, slslyr, sls3ago, and falord_bin (due to VIF issues)
fit.log9 = glm(formula = targdol_bin ~ sqrt(yrs_since_add) + sqrt(yrs_since_lp) + 
                 sqrt(ordtyr) + sqrt(ordlyr) + sqrt(ord2ago) + sqrt(ord3ago) + 
                 sqrt(ordlyr * ord2ago) + sqrt(ord2ago * ord3ago) + 
                 sqrt(ordtyr * ordlyr * ord2ago * ord3ago) + log10(slshist + 1) + 
                 sqrt(ordtyr * ordlyr) + sqrt(ordhist1 * sprord) + 
                 sqrt(falord_bin/(yrs_since_lp + 1)) + sqrt(ordhist1/(yrs_since_add + 1)), 
               family = binomial, data = Catalog.log.train, na.action = na.exclude)

summary(fit.log9)
# AIC = 24826 (14 predictors)

vif(fit.log9)
# All VIFs < 10

write.csv(vif(fit.log5),"vif_log5.csv")
write.csv(vif(fit.log9),"vif_log9.csv")


######## Don't use the following because it takes too long

# # step using AIC (both LR and LM) ####
# library(MASS)
# fit_stepAIC = step(object = full, direction = "both")  # AIC
# 
# # step using BIC (both LR and LM) ####
# fit_stepAIC = step(object = full, direction = "both", k=log(n))  # BIC
# 
# # use single drop/add (both LR and LM) ####
# drop1(fit, test="Chisq",data=mydata) # LRT on individual terms
# add1(fit, scope= ~ X1 + X2,test="Chisq",data=mydata) # LRT on individual terms

# # use bestglm (both LM and LR) #### (takes a long time, ~ 5 min for 7 var )
# source("ModelSelection.R")
# lr_select = bestglm_binomial(f,Catalog.log.train)
# 
# # use glmulti (both LM and LR) #### (takes a long time, ~ 5 min for 7 var )
# lr_select2 = glmulti_binomial(f,Catalog.log.train)

##########


# use bic.glm (both LR and LM) ####
# http://www.medicine.mcgill.ca/epidemiology/joseph/courses/EPIB-621/logselect.pdf
# Bayesian Model Averaging accounts for the model uncertainty inherent in the variable 
# selection problem by averaging over the best models in the model class according 
# to approximate posterior model probability.
# install.packages("BMA")
library(BMA)
f=formula(fit.log1)
output=bic.glm(f,data=Catalog.log.train,glm.family=binomial)
summary(output)
names(output)

output$label # variables in the model
output$postprob # best model = highest posterior probability
output$probne0 # probability variable should be in the model
write.csv(output$probne0, "probInModel_log.csv")


# LR Model misclasssification #############################################

######### Jason:
Predictions = predict(fit.log5, newdata = Catalog.log.test, type = "response")

bin.predict = NULL
for (i in 1:length(Predictions)) {
  if (Predictions[i] > 0.5) {
    bin.predict[i] = 1
  }
  else {
    bin.predict[i] = 0
  }
}

predict.test = NULL
for (i in 1:length(Predictions)) {
  if (bin.predict[i] == Catalog.log.test[i,20]) {
    predict.test[i] = 1
  }
  else {
    predict.test[i] = 0
  }
}

Misclass_Rate = 1 - mean(predict.test)
Misclass_Rate
# 0.0756707

Table = cbind(Catalog.log.test[,20], predict.test)
colnames(Table) = c("targdol_bin", "predict.test")

Misclass_Rate0 = 1 - mean(Table[Table[,1]==0, 2])
Misclass_Rate0
# 0.006856847
#########

classTable = function(response,pred_prob,cutoff){
  predicted = pred_prob>cutoff
  tab = table(response, predicted) # >cutoff classify as defaulter
  colnames(tab)=c(0,1)
  tab_margin= round(prop.table(tab,1),4) # row percents, condition observed true value
  correct = round(sum(diag(tab))/sum(tab),4) # classification rate correct
  misclass = 1-correct
  
  # cutoff depends on type I , type II error
  # spam : false + most expensive
  # airport: false - most expensive
  # cost vs cutoff probability
  # for different cost of hit, false hit, etc
  
  v=list(tab=tab,
         tab_margin=tab_margin,
         overall_class=correct,
         overall_misclass=misclass)
  return(v)
  
}


fit=fit.log5

# classification tables based on 0.5 cutoff

train_table= classTable(response = Catalog.log.train$targdol_bin,
                        pred_prob = fit$fitted.values,
                        cutoff=0.5)
train_table$tab
train_table$tab_margin
train_table$overall_class
train_table$overall_misclass

phat = predict(fit, newdata=Catalog.log.test, type="resp") # score test set
test_table = classTable(response = Catalog.log.test$targdol_bin,
                        pred_prob = phat,
                        cutoff=0.5)
test_table$tab
test_table$tab_margin
test_table$overall_class
test_table$overall_misclass

write.csv(test_table$tab,"test_tab.csv")
write.csv(test_table$tab_margin,"test_tab_margin.csv")

# compute classification for different cutoffs

classCutoffs = function(response,pred_prob,cutoffs){
  tabs = lapply(cutoffs,function(x) classTable(response,pred_prob,as.numeric(x)))
  overall_misclass = sapply(1:length(cutoffs),function(x) tabs[[x]]$overall_misclass)
  misclass1as0 = sapply(1:length(cutoffs),function(x) tabs[[x]]$tab_margin[2])
  n01 = sapply(1:length(cutoffs),function(x) tabs[[x]]$tab[3])
  n10 = sapply(1:length(cutoffs),function(x) tabs[[x]]$tab[2])
  results = data.frame(cutoffs,overall_misclass,misclass1as0,n01,n10)                        
  return(results)
  
}

cutoffs = seq(0.1,0.9,0.1)
train_cutoffs = classCutoffs(Catalog.log.train$targdol_bin,fit$fitted.values,cutoffs)
train_cutoffs
write.csv(train_cutoffs,"train_cutoffs.csv")


c10 = mean(Catalog.lm.train$targdol_pur) # false neg, type II error, missing a true buyer
c10 = 40
c01 = 20           # false pos, type I error, send catalog to a not a true buyer

train_cutoffs=cbind(train_cutoffs,cost=train_cutoffs$n01*c01+train_cutoffs$n10*c10)
train_cutoffs

plot(train_cutoffs$cutoffs,train_cutoffs$cost,pch=16,xlab="cutoffs",ylab="cost")
lines(train_cutoffs$cutoffs,train_cutoffs$cost)

# example of how we can decide the cutoff,which depends on teh cost of type I , type II error

# financial criterion ###########################################################
Predictions.lm = predict(fit.lm4, newdata = Catalog.log.test, type = "response")

Expected.Value = Predictions*10^(Predictions.lm - 1)

Catalog.lm.test1 = cbind(Catalog.lm.test, Expected.Value)

Catalog.lm.test2 = Catalog.lm.test1[order(Catalog.lm.test1[,21], decreasing = TRUE),]


Catalog.lm.test2 = Catalog.lm.test2[1:5000,]

sum(Catalog.lm.test2$targdol_pur, na.rm = TRUE)
# 112905.9

sum(Catalog.lm.test$targdol_pur, na.rm = TRUE)
# 226456.3

sum(Catalog.lm.test2$targdol_pur, na.rm = TRUE)/sum(Catalog.lm.test$targdol_pur, na.rm = TRUE)
# 0.4985771

# Following his instructions:
# Dear Students: Instead of identifying the top 10,000 prospective buyers 
# according to your model, please identify only the top 5000 buyers 
# (because the test set has < 5000 actual buyers). 
# Then add up their actual targdol's which is the payoff according to your model. In other words, had you used your model
# to target these top 5000 prospects then that would have been your actual sales revenue.

# note that rownames is different than row number
selected_cutoff=0.5
id_target = as.numeric(names(Predictions[(Predictions>selected_cutoff)]))
length(id_target) # number of predicted buyers
hist(Predictions)

Catalog.lm.test_target = cbind(Catalog.lm.test,pred=Predictions.lm)
id_row = as.numeric(rownames(Catalog.lm.test_target))
Catalog.lm.test_target = Catalog.lm.test_target[(id_row %in% id_target),]  #predicted buyers
dim(Catalog.lm.test_target)

top5000_pred_targdol = Catalog.lm.test_target[order(Catalog.lm.test_target$pred, decreasing=TRUE),][1:5000,"targdol_pur"]
sum_actual_targodl5000 = sum(top5000_pred_targdol,na.rm=TRUE)
sum_actual_targodl5000 

p=dim(model.matrix(fit.lm4))[2]-1 # - intercetp
MSEP = (sum((Catalog.lm.test_target$pred-Catalog.lm.test_target$targdol_pur)^2,na.rm=T))/(length(id_target) - p +1)
MSEP

# need to convert targdol_pur NA to zero
Catalog.lm.test_target2=Catalog.lm.test_target
Catalog.lm.test_target2$targdol_pur = ifelse(is.na(Catalog.lm.test_target2$targdol_pur),0,Catalog.lm.test_target2$targdol_pur)
MSEP = (sum((Catalog.lm.test_target2$pred-Catalog.lm.test_target2$targdol_pur)^2))/(length(id_target) - p +1)
MSEP



     
hist(Catalog.lm.test_target$pred)


# LR Model diagnostics ##############################################################

f1=formula(fit.log5)
f2=formula(fit.log9)

# Null deviance = left unexplained after fittings beta's Bigger difference
# -> more explained How much addtw two predictors explain = 79

# 6 = 6 constraints H0: beta 1 = beta 2 ...beta 6
fit0 = glm(targdol_bin ~ 1, family = binomial(link = "logit"), data = Catalog.log.train)
anova(fit0, fit.log5, test = "Chisq")
anova(fit0, fit.log9, test = "Chisq")

# The test of overall goodness of fit shows that both models indicate rejection of the
# null hypothesis that all the coefficients are equal to zero. 

# ROC curves
source("ROC.R")
plotROC2(f1,f2,Catalog.log.train)
# plotROC1(f1,Catalog.log.train)

# The AUC (or concordance index) is a diagnostic test of the discriminatory power of the model. 
# In this case both models have an AUC > 80%, meaning the models can be regarded as very 
# good in terms of discriminatory power. The p-value suggests that the AUC's are statistically
# significanly different, however, in practical terms there is not much difference. 

fit = fit.log5
plot(fit,which=c(1),ask=FALSE)

res_per=residuals(fit,type="pearson")
plot(fit$fitted.values,res_per)

res_dev=residuals(fit,type="deviance")
plot(fit$fitted.values,res_dev)

influencePlot(fit)

names(influence.measures(fit))

names(influencePlot(fit)$infmat)
# Can also get studeres, hat and cook D :
head(influence.measures(fit)$is.inf)

write.csv(model.frame(fit.log5),"catalog_log5_train.csv")
write.csv(model.frame(fit.log9),"catalog_log9_train.csv")
summary(fit.log5)
# Generalized linear models are freed from the assumption that residuals are normally distributed with equal variance,
# but the method nevertheless makes important assumptions that should be checked.
