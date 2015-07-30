####################
# Class: STAT 428  #
# Date: 04/15/13   #
# Project          #
# Luis Steven Lin  #
####################

########################################################################
# Terminology

# loadings or rotations = principal component
# 1st PC = direction of maximum spread
# scores or 'x' (data%*%rotation)= projecttion of data points
# along the PCs (rotated data) = predict(my.pca)
# LDA
# loadings/coeff of the discriminant functions = 'scaling'
# discriminant functions = predict(my.lda)$x

########################################################################
# Data Info

# rows = checmial analytes
# 147 chemical analytes can be separated into 21 groups 
# with each group has 7 analytes 
# colunms = 108 colors


########################################################################
# Load data

getwd()
data=read.csv('DB_220_IDLH_2mins.txt', header=F, row.names=1, sep='\t')

# Examine data
dim(data)
rownames(data)
colnames(data)
data[1:14,1:5]

########################################################################
# Data Preparation

# get group labels 
y = rownames(data)

# replace "-" with "_" to have the same pattern
y = gsub("-","_",y,fixed=TRUE)

# Keep the group name only (string before "_")
y = sapply(strsplit(y,"_",fixed = TRUE),"[[",1)

# String as levels
y = as.factor(y)

# Bind y to data
data = cbind(y,data)

# Function to randomly pick 1 analyte from each group

randomIndices = function(seed=F){
	if(seed==T){set.seed(1)}
	indicesTest = rep(0,21)
	for(i in 1:21){	
		indicesTest[i] = sample(7,1) + (i-1)*7
	}
	return(indicesTest)
}

# Randomly pick 1 analyte from each group
# indicesTest=randomIndices()

# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']

########################################################################
# Linear Discriminant Analysis (LDA)

library(MASS)

# Start the clock!
ptm <- proc.time()

errorRate.LDA = rep(0,100)
errorID.LDA = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.LDA)=yTestNames
colnames(errorID.LDA)="Errors"

# Run LDA 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group
	indicesTest=randomIndices()

	# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct LDA model with training set
	my.lda=lda(y~.,data=dataTrain)

	# Use LDA to predict group of testing data
	my.pred=predict(my.lda,dataTest)

	# Compute misclassification error rate and save
	miss = my.pred$class!=yTest
	errorRate.LDA[i] = sum(miss)/length(yTest)
	
	# Find which groups were misclassified in the test data
	errorID.LDA = errorID.LDA + as.numeric(miss)
}

# Average misclassfication rate (0.04809524)
meanError.LDA  = mean(errorRate.LDA)
meanError.LDA 

# Double check
sum(errorID.LDA)/(21*100)

# Misclassified groups in test data
errorID.LDA

# Stop the clock (9.16)
proc.time() - ptm

########################################################################
# Logistic Regression

# Start the clock!
ptm <- proc.time()

errorRate.LOG = rep(0,100)
errorID.LOG = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.LOG)=yTestNames
colnames(errorID.LOG)="Errors"
for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group
	indicesTest=randomIndices()

	# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']

# Run 21 different logistic regressions for the 21 groups

prob=matrix(0,21)
for(j in 1:21){
	y = rep(0,6*21)
	y[((j-1)*6+1):((j-1)*6+6)]=1

	# Substitute group labels with 1 for current group, 0 for the rest
	dataTrain[,1]=y

	# Run logistic regression for the current group with training data
	my.glm = glm(y~.,family=binomial,data=dataTrain)

	# Predict probability that the test data of the current group 
	# falls in the current group
	my.pred = predict(my.glm,newdata = dataTest, type="response")
	prob=cbind(prob,my.pred)
	}

# Find classification of test data: for each group in test data (row), 
# choose the group with highest probability (column)
# This probability represents the predicted probability of belonging 
# to that group vs. other
prob = prob[,-1]
rownames(prob) = yTestNames
colnames(prob) = yTestNames
yClass=yTest
for(j in 1:21){
	index=which(max(prob[j,])==prob[j,])
	yClass[j] = yTest[index]
	}
miss = yClass!=yTest

# Compute misclassification error rate and save
errorRate.LOG[i] = sum(miss)/length(yTest)


# Find which groups were misclassified in the test data
errorID.LOG = errorID.LOG + as.numeric(miss)
}

# Average misclassfication rate (0.1219048)
meanError.LOG = mean(errorRate.LOG)
meanError.LOG

# Double Check
sum(errorID.LOG)/(21*100)

# Misclassified groups in test data
errorID.LOG

# Stop the clock (151.79)
proc.time() - ptm

########################################################################
# Support Vector Machine (SVM)

# Install package e1071
install.packages("e1071")
library(e1071)

# Start the clock!
ptm <- proc.time()

errorRate.SVM = rep(0,100)
errorID.SVM = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.SVM)=yTestNames
colnames(errorID.SVM)="Errors"

# Run SVM 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group
	indicesTest=randomIndices()

	# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct SVM model with training set
	my.svm = svm(y~.,data=dataTrain)
	
	# Use SVM to predict group of testing data
	my.pred = predict(my.svm,dataTest)
	
	# Compute misclassification error rate and save
	miss = my.pred!=yTest
	errorRate.SVM[i] = sum(miss)/length(yTest)
	
	# Find which groups were misclassified in the test data
	errorID.SVM = errorID.SVM + as.numeric(miss)
}

# Average misclassfication rate (0.08857143)
meanError.SVM = mean(errorRate.SVM)
meanError.SVM 

# Double Check
sum(errorID.SVM)/(21*100)

# Misclassified groups in test data
errorID.SVM

# Stop the clock (7.11)
proc.time() - ptm

########################################################################
# Classification and Regression Tree (CART)

# Install package tree
install.packages("tree")
library(tree)

# Start the clock!
ptm <- proc.time()

errorRate.CART = rep(0,100)
errorID.CART = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.CART)=yTestNames
colnames(errorID.CART)="Errors"

# Run CART 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group
	indicesTest=randomIndices()

	# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct CART model with training set
	my.tree=tree(y~.,data=dataTrain)
	
	# Use CART to predict group of testing data
	my.pred=predict(my.tree,dataTest,type='class')
	
	# Compute misclassification error rate and save
	miss = my.pred!=yTest
	errorRate.CART[i] = sum(miss)/length(yTest)
	
	# Find which groups were misclassified in the test data
	errorID.CART = errorID.CART + as.numeric(miss)
}

# Average misclassfication rate (0.142381)
meanError.CART = mean(errorRate.CART)
meanError.CART 

# Double Check
sum(errorID.CART)/(21*100)

# Misclassified groups in test data
errorID.CART

# Stop the clock (6.06)
proc.time() - ptm

########################################################################
# Random Foreset

# Install package random forest
install.packages("randomForest")
library(randomForest)

# Start the clock!
ptm <- proc.time()

errorRate.RF = rep(0,100)
errorID.RF = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.RF)=yTestNames
colnames(errorID.RF)="Errors"

# Run RF 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group
	indicesTest=randomIndices()

	# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct RF model with training set
	my.RF = randomForest(y~.,data=dataTrain)
	
	# Use RF to predict group of testing data
	my.pred=predict(my.RF,dataTest,type='class')
	
	# Compute misclassification error rate and save
	miss = my.pred!=yTest
	errorRate.RF[i] = sum(miss)/length(yTest)
	
	# Find which groups were misclassified in the test data
	errorID.RF = errorID.RF + as.numeric(miss)
}

# Average misclassfication rate (0)
meanError.RF = mean(errorRate.RF)
meanError.RF 

# Double Check
sum(errorID.RF)/(21*100)

# Misclassified groups in test data
errorID.RF

# Stop the clock (39.28)
proc.time() - ptm

########################################################################
# Boosting

# Install adabag package
install.packages("adabag")
library(adabag)


# Start the clock!
ptm <- proc.time()

errorRate.BOOST  = rep(0,100)
errorRate.BOOST2 = rep(0,100)
errorID.BOOST = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.BOOST)=yTestNames
colnames(errorID.BOOST)="Errors"

# Run BOOST 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group
	indicesTest=randomIndices()

	# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct BOOST model with training set
	my.BOOST = boosting(y~.,data=dataTrain)
	
	# Use BOOST to predict group of testing data
	my.pred = predict(my.BOOST,newdata=dataTest)

	# Compute misclassification error rate and save
	miss = my.pred$class!=yTest
	errorRate.BOOST[i] = sum(miss)/length(yTest)
	errorRate.BOOST2[i] = my.pred$error
	
	# Find which groups were misclassified in the test data
	errorID.BOOST = errorID.BOOST + as.numeric(miss)
}

# Average misclassfication rate (0.02095238)
meanError.BOOST = mean(errorRate.BOOST)
meanError.BOOST
mean(errorRate.BOOST2) 

# Double Check
sum(errorID.BOOST)/(21*100)

# Misclassified groups in test data
errorID.BOOST

# Stop the clock (elapsed = 2013.95 sec)
proc.time() - ptm


########################################################################
# Summary

errorIDs=cbind(errorID.LDA, errorID.LOG, errorID.SVM,
	         errorID.CART, errorID.RF, errorID.BOOST)
colnames(errorIDs) = c("LDA","LOG","SVM","CART","RF","BOOST")

meanErrors=cbind(meanError.LDA, meanError.LOG, meanError.SVM,
	         meanError.CART, meanError.RF, meanError.BOOST)
colnames(meanErrors) = c("LDA","LOG","SVM","CART","RF","BOOST")

times = cbind(9.16,151.79,7.11,6.06,39.28,2013.95)
colnames(times) = c("LDA","LOG","SVM","CART","RF","BOOST")

colnames(meanErrors) = c("LDA","LOG","SVM","CART","RF","BOOST")

summary=rbind(meanErrors,times)
rownames(summary)=c("meanError","CPUtime")

summary
errorIDs

# Export
write.csv(summary, "k:/summary.csv")
write.csv(errorIDs, "k:/errorIDs.csv")

# Display heat map
install.packages("gplots")
library("gplots")
heatmap.2(errorIDs,key=TRUE,trace="none",density.info="none",
		col=greenred(75) )

# Save heat map
bmp(filename="k:/heatMapCLASS.bmp")
heatmap.2(errorIDs,key=TRUE,trace="none",density.info="none",
		col=greenred(75) )
dev.off()


########################################################################
# Take out two repeats from each chemical class (one for testting)

# Function to randomly pick 1 analyte to remove from each group
# and 1 analyte for test data from each group
indices = function(seed=F){
	if(seed==T){set.seed(1)}
	indicesRemove1 = rep(0,21)
	indicesRemove2 = rep(0,21)
	indicesTest = rep(0,21)
	for(i in 1:21){	
		indicesRemove1[i] = sample(7,1) + (i-1)*7
		indicesRemove2[i] = sample(6,1) + (i-1)*6
		indicesTest[i] = sample(5,1) + (i-1)*5
	}
	return(list("remove1"=indicesRemove1,"remove2"=indicesRemove2,
			"test"=indicesTest))
}

########################################################################
# Reapeat Linear Discriminant Analysis (LDA) with removed data

library(MASS)

# Start the clock!
ptm <- proc.time()

errorRate.LDA2 = rep(0,100)
errorID.LDA2 = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.LDA2)=yTestNames
colnames(errorID.LDA2)="Errors"

# Run LDA 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group to remove
	# and 1 for test data
	index=indices()

	# Remove analyte from data
	data2 = data[-index$remove1,]
	data2 = data2[-index$remove2,]

	# Testing and training data set
	dataTest = data2[index$test,]
	dataTrain = data2[-index$test,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct LDA model with training set
	my.lda=lda(y~.,data=dataTrain)

	# Use LDA to predict group of testing data
	my.pred=predict(my.lda,dataTest)

	# Compute misclassification error rate and save
	miss = my.pred$class!=yTest
	errorRate.LDA2[i] = sum(miss)/length(yTest)
	
	# Find which groups were misclassified in the test data
	errorID.LDA2 = errorID.LDA2 + as.numeric(miss)
}

# Average misclassfication rate (0.04809524)
meanError.LDA2  = mean(errorRate.LDA2)
meanError.LDA2 

# Double check
sum(errorID.LDA2)/(21*100)

# Misclassified groups in test data
errorID.LDA2

# Stop the clock (9.16)
proc.time() - ptm


########################################################################
# Principal Component Analysis to reduce dimensions

	# Randomly pick 1 analyte from each group to remove
	# and 1 for test data
	index=indices()

	# Remove analyte from data
	data2 = data[-index$remove1,]
	data2 = data2[-index$remove2,]

	# Use built-in R function for PCA
	my.pca = prcomp(data2[,-1])

	# Get the cumulative propotions of PC
	cumProp = summary(my.pca)$imp["Cumulative Proportion",]
	cumProp

	# Output the cumulative scree plot of first, 25 PC's
	plot(1:25,cumProp[1:25],type="o", pch=16,
	xlab="Principal Components",
	ylab="Proportion of variance explained",
	main="Cumulative Screeplot of the first 25 PCs")

	# Save plot
	bmp(filename="k:/screePlot.bmp")
	plot(1:25,cumProp[1:25],type="o", pch=16,
	xlab="Principal Components",
	ylab="Proportion of variance explained",
	main="Cumulative Screeplot of the first 25 PCs")
	dev.off()

	# Get rotated data (scores) of the first 14 PC s
	scores = my.pca$x[,1:14]


	# Note that my.pca$x, predict(my.pca, data) and predict(data)
	# are all equivalent. 

	# Create reduced data
	dataR = data2[,1:2]  
	#need to keep V2 because if not y is converted 
	#to numeric and changes col name when bind with scores
	dataR = cbind(dataR,scores)
	dataR = dataR[,-2]

########################################################################
# Reapeat Linear Discriminant Analysis (LDA) with reduced data

library(MASS)

# Start the clock!
ptm <- proc.time()

errorRate.LDAR = rep(0,100)
errorID.LDAR = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.LDAR)=yTestNames
colnames(errorID.LDAR)="Errors"

# Run LDA 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group to remove
	# and 1 for test data
	index=indices()

	# Remove analyte from data
	data2 = data[-index$remove1,]
	data2 = data2[-index$remove2,]

	# Use built-in R function for PCA
	my.pca = prcomp(data2[,-1])

	# Get rotated data (scores) of the first 14 PC s
	scores = my.pca$x[,1:14]

	# Create reduced data
	dataR = data2[,1:2]  
	#need to keep V2 because if not y is converted 
	#to numeric and changes col name when bind with scores
	dataR = cbind(dataR,scores)
	dataR = dataR[,-2]

	# Testing and training data set
	dataTest = dataR[index$test,]
	dataTrain = dataR[-index$test,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct LDA model with training set
	my.lda=lda(y~.,data=dataTrain)

	# Use LDA to predict group of testing data
	my.pred=predict(my.lda,dataTest)

	# Compute misclassification error rate and save
	miss = my.pred$class!=yTest
	errorRate.LDAR[i] = sum(miss)/length(yTest)
	
	# Find which groups were misclassified in the test data
	errorID.LDAR = errorID.LDAR + as.numeric(miss)
}

# Average misclassfication rate (0.04809524)
meanError.LDAR  = mean(errorRate.LDAR)
meanError.LDAR

# Double check
sum(errorID.LDAR)/(21*100)

# Misclassified groups in test data
errorID.LDAR

# Stop the clock (9.16)
proc.time() - ptm


########################################################################
# Reapeat Linear Discriminant Analysis (LDA) with reduced data
# using different number of PCs

library(MASS)

# Start the clock!
ptm <- proc.time()

meanError.LDAR = rep(0,14)
errorIDs.LDAR = matrix(0,21)

# Run LDA using 1 to 14 PCs and compute the error
for(n in 1:14){

errorRate.LDAR = rep(0,100)
errorID.LDAR = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.LDAR)=yTestNames
colnames(errorID.LDAR)="Errors"

# Run LDA 100 times and compute the error

for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group to remove
	# and 1 for test data
	index=indices()

	# Remove analyte from data
	data2 = data[-index$remove1,]
	data2 = data2[-index$remove2,]

	# Use built-in R function for PCA
	my.pca = prcomp(data2[,-1])

	# Get rotated data (scores) of the first n PC s
	scores = my.pca$x[,1:n]

	# Create reduced data
	dataR = data2[,1:2]  
	#need to keep V2 because if not y is converted 
	#to numeric and changes col name when bind with scores
	dataR = cbind(dataR,scores)
	dataR = dataR[,-2]

	# Testing and training data set
	dataTest = dataR[index$test,]
	dataTrain = dataR[-index$test,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']
	
	# Construct LDA model with training set
	my.lda=lda(y~.,data=dataTrain)

	# Use LDA to predict group of testing data
	my.pred=predict(my.lda,dataTest)

	# Compute misclassification error rate and save
	miss = my.pred$class!=yTest
	errorRate.LDAR[i] = sum(miss)/length(yTest)
	
	# Find which groups were misclassified in the test data
	errorID.LDAR = errorID.LDAR + as.numeric(miss)
}

# Average misclassfication rate (0.04809524)
meanError.LDAR[n]  = mean(errorRate.LDAR)

# Misclassified groups in test data
errorIDs.LDAR = cbind(errorIDs.LDAR,errorID.LDAR)

}

# Average misclassfication rate
meanError.LDAR=as.matrix(meanError.LDAR)
rownames(meanError.LDAR)= paste(rep("PC",14),1:14,sep="")
colnames(meanError.LDAR)= "meanError"
meanError.LDAR

# Double check for last PC run
sum(errorID.LDAR)/(21*100)

# Misclassified groups in test data
errorIDs.LDAR = errorIDs.LDAR[,-1]
colnames(errorIDs.LDAR)= paste(rep("PC",14),1:14,sep="")
errorIDs.LDAR

# Stop the clock (9.16)
proc.time() - ptm

########################################################################
# Export

cumProp
meanError.LDAR
errorIDs.LDAR

# Export
write.csv(cumProp, "k:/cumProp.csv")
write.csv(meanError.LDAR, "k:/meanErrorLDAR.csv")
write.csv(errorIDs.LDAR, "k:/errorIDsLDAR.csv")

# Display heat map
install.packages("gplots")
library("gplots")
heatmap.2(errorIDs.LDAR,key=TRUE,trace="none",density.info="none",
		col=greenred(75),Colv=F) # Colv=F don't cluster columns

# Save heat map
bmp(filename="k:/heatMapPC.bmp")
heatmap.2(errorIDs.LDAR,key=TRUE,trace="none",density.info="none",
		col=greenred(75),Colv=F) # Colv=F don't cluster columns
dev.off()

#########################################################################
# Manual Code for LDA (from lab2)

#get same result by diy without using the lda fcn in R
gmean=by(x,y,mean)

#make list as matrix
gmean=matrix(unlist(gmean),ncol=2)
gmean

#compare to R
names(my.lda)
my.lda$means
gmean==t(my.lda$means)

#compute sigma b
gvar=var(t(gmean))

#compute sigma w, by taking avg of the four estimates, 
#by law of large number to get a smaller variance
varw=by(x,y,var)
varw1=(varw[[1]]+varw[[2]])/2

#M is asymmtric but it should symmetric
#compute in another way

temp=svd(varw1)
names(temp)
vareta=temp$u%*%diag(1/sqrt(temp$d))%*%t(temp$v)
M=vareta%*%gvar%*%vareta
ldasvd=svd(M)

# eigenvector
ldasvd$u  

# coefficient
my.lda$scaling
LDAscale=vareta%*%ldasvd$u

ldaproj=as.matrix(x)%*%LDAscale[,1:2]


#########################################################################
#########################################################################
#multinom
install.packages("VGAM")
library(VGAM)
glm(y~.,family=multinomial,data=dataTrain)
my.MLOG = multinom(y~.,data=dataTrain)
my.pred = predict(my.MLOG,newdata=dataTest,type="prob")
length(my.pred)

########################################################################
# Logistic Regression (other misclassification type)

# Start the clock!
ptm <- proc.time()

errorRate.LOG = rep(0,100)
errorID.LOG = matrix(0,21)

	# get group labels 
	yTestNames = rownames(dataTest)

	# replace "-" with "_" to have the same pattern
	yTestNames  = gsub("-","_",yTestNames,fixed=TRUE)

	# Keep the group name only (string before "_")
	yTestNames  = sapply(strsplit(yTestNames,"_",fixed = TRUE),"[[",1)


rownames(errorID.LOG)=yTestNames
colnames(errorID.LOG)="Errors"
for(i in 1:100){
	
	set.seed(i)

	# Randomly pick 1 analyte from each group
	indicesTest=randomIndices()

	# Testing and training data set
	dataTest = data[indicesTest,]
	dataTrain = data[-indicesTest,]
	yTest = dataTest[,'y']
	yTrain = dataTrain[,'y']

# Run 21 different logistic regressions for the 21 groups
miss=0
prob=rep(0,21)
for(j in 1:21){
	y = rep(0,6*21)
	y[((j-1)*6+1):((j-1)*6+6)]=1

	# Substitute group labels with 1 for current group, 0 for the rest
	dataTrain[,1]=y

	# Run logistic regression for the current group with training data
	my.glm = glm(y~.,family=binomial,data=dataTrain)

	# Predict probability that the test data of the current group 
	# falls in the current group (cutoff=0.5)
	my.pred = predict(my.glm,newdata = dataTest[j,], type="response")
	prob[j] = my.pred
	}

# If it doesn't fall in corresponding group, then  misclassification 
miss = prob<=0.5

# Compute misclassification error rate and save
errorRate.LOG[i] = sum(miss)/length(yTest)


# Find which groups were misclassified in the test data
errorID.LOG = errorID.LOG + as.numeric(miss)
}

# Average misclassfication rate (0.01380952)
meanError.LOG = mean(errorRate.LOG)
meanError.LOG 

# Double check
sum(errorID.LOG)/(21*100)

# Misclassified groups in test data
errorID.LOG

# Stop the clock (154.88)
proc.time() - ptm
