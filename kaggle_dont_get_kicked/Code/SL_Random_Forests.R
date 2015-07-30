setwd("/sscc/home/a/amk202/SuddenLink/predictive")
# Import data
filename = "/sscc/home/a/amk202/Predictive/car_train.csv"
mydata = read.csv(filename,header = T)

filename = "/sscc/home/a/amk202/Predictive/car_test.csv"
test = read.csv(filename,header = T)

str(mydata)

var_drop = c("PurchDate","VehYear","Model", "Trim","SubModel","PRIMEUNIT","VNST")

mydata = mydata[!(names(mydata) %in% var_drop)]

var_cont = c( "VehicleAge","VehOdo", 
              "MMRAcquisitionRetailAveragePrice","VehBCost",
              "WarrantyCost", "Differential" )

var_drop = c("MMRAcquisitionAuctionAveragePrice",
             "MMRAcquisitionAuctionCleanPrice",
             "MMRAcquisitonRetailCleanPrice",
             "MMRCurrentAuctionAveragePrice",
             "MMRCurrentAuctionCleanPrice",
             "MMRCurrentRetailAveragePrice",
             "MMRCurrentRetailCleanPrice",
             "moved_total","moved_diff_county",
             "moved_diff_state","median_income","mean_income",
             "total_population", "high_school", "bachelor", "urban",
             "rural")

mydata = mydata[!(names(mydata) %in% var_drop)]

names(mydata)

resp = "IsBadBuy"
pred = names(mydata)[names(mydata)!=resp]
mydata[,resp] = as.factor(mydata[,resp])

# sample to test code
#mydata = mydata[sample(rownames(mydata),5000,replace=FALSE),]

#### Scale: Standardize Predictors ####

#standardize = function( x, newdata, data ) {
  #( newdata[ , x ] - mean( data[ , x ] ) ) / sd( data[ , x ] );
#}

#standardize("bachelor",test,mydata)

#mydata_pre_scale = mydata
#mydata[,var_cont]= sapply(mydata[,var_cont], function(x) (x-mean(x))/sd(x)) #standardize predictors
#test[,var_cont]= sapply(test[,var_cont], function(x) (x-mean(x))/sd(x)) #standardize predictors
# or use:
# scale(mydata[,var_cont])


#### GINI ####

gini = function ( actual, predicted ) {
  n = length( actual );
  gini_data = as.data.frame( cbind( actual, predicted, row = 1:n ) );
  gini_data = gini_data[ with( gini_data, order( -predicted, row ) ), ];
  sum( cumsum( gini_data[ , 1 ] ) / sum( gini_data[ , 1 ] ) - ( 1:n ) / n ) / n;
}

#### Random Forest ####

library (randomForest)

# Tune parameters
ptm <- proc.time()
tuneRandomForest = tuneRF(mydata[,pred], mydata[,resp], ntreeTry = 300, stepFactor=1.5)
proc.time() - ptm

# Start the clock!
ptm <- proc.time()

out =randomForest(IsBadBuy~.,data=mydata,ntree=300,mtry=3,nodesize=5 ,importance =TRUE)

# Stop the clock
proc.time() - ptm

# train
y = mydata[,resp]
yhat = predict(out,type="class")
phat = predict(out,type="prob")[,2]
sum(y != yhat)/length(y)

# test
y = test[,resp]
y = as.numeric(y==1)
yhat = predict(out,newdata= test, type="class")
phat = predict(out,newdata= test, type="prob")[,2]
sum(y != yhat)/length(y)
gini(y,phat)

varImpPlot (out)

# install.packages("pROC")
library(pROC)
plot.roc(y,phat)

roc=roc(y,phat,percent=TRUE)
auc(roc)


# Start the clock!
ptm <- proc.time()

Nrep = 10
n=nrow(mydata) 
y = mydata[,resp]

Ntrees = seq(100,500,100)
Npred= seq(1,10,1)

#n iterations , 30 sper iteraion
Nrep*length(Ntrees)*length(Npred)
loops = Nrep*length(Ntrees)*length(Npred) # without folds

#misclass= matrix(0,Nrep*length(lambdas)*length(models),4) # misclass for each model and each replicate
misclass = c()

for (j in 1:Nrep) {
 
  for (m in Ntrees){
    
    yhat = y; # reset yhat (actually don't need this)
    
    for(l in Npred){
      out =randomForest(IsBadBuy~.,data=mydata,ntree=m,nodesize=l, importance =TRUE)
        
      yhat = predict(out,type="class")
      phat = predict(out,type="prob")[,2]
      
      misclass = rbind(misclass,c(j,m,l,sum(y!=yhat)/length(y)))
      
      loops = loops - 1
      print(paste("Remaining Iterations: ",loops))
      print(misclass[nrow(misclass),])
      write(misclass[nrow(misclass),],file="myfile.csv",append=TRUE)
      
    }# end of l loop
      
  }# end of m loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

colnames(misclass) = c("rep","Ntree","Npred","misclass")
misclass
misclassAVE = aggregate(misclass~ Ntree + Npred, misclass, mean ) # aggregate reps
misclassAVE = misclassAVE[order(misclassAVE[,"Ntree"]),] # sort
misclassAVE

param_opt = misclassAVE[which.min(misclassAVE[,"misclass"]),]
param_opt

write.csv(param_opt, file = "best_RandomForests.csv",row.names=FALSE)

write.csv(misclassAVE, file = "misclassAVE__RandomForests.csv",row.names=FALSE)

#### Use best ####

test = read.csv(filename,header = T)
#mydata[,resp] = as.factor(mydata[,resp])
#test[,var_cont]= sapply(test[,var_cont], function(x) (x-mean(x))/sd(x)) #standardize predictors

# Start the clock!
ptm <- proc.time()

out =randomForest(IsBadBuy~.,data=mydata,
                  ntree=as.numeric(param_opt["Ntree"]),
                  nodesize=as.numeric(param_opt["Npred"]), importance =TRUE)

# Stop the clock
proc.time() - ptm

# train
y = mydata[,resp]
yhat = predict(out,type="class")
phat = predict(out,type="prob")[,2]
sum(y != yhat)/length(y)

t = table(y,yhat)
prop.table(t,1)

# test
y = test[,resp]
y = as.numeric(y==1)
yhat = predict(out,newdata= test, type="class")
phat = predict(out,newdata= test, type="prob")[,2]
sum(y != yhat)/length(y)
gini(y,phat)

# install.packages("pROC")
library(pROC)
plot.roc(y,phat)

roc=roc(y,phat,percent=TRUE)
auc(roc)

out$importance
importance(out)
varImpPlot(out)

title = paste(c("Variable Importance Plot using Random Forests"),sep="",collapse="")
save_name = paste(c("RF_Plot",".jpg"),sep="",collapse="")
jpeg(save_name)
varImpPlot (out)
dev.off()



