setwd("/sscc/home/a/amk202/SuddenLink/predictive")
# Import data
filename = "/sscc/home/a/amk202/Predictive/car_train.csv"
mydata = read.csv(filename,header = T)

filename = "/sscc/home/a/amk202/Predictive/car_test.csv"
test = read.csv(filename,header = T)


str(mydata)
table(mydata$PRIMEUNIT) # lot of null
table(mydata$AUCGUART)  # lot of null

drop_var = c("PurchDate","VehYear","Model", "Trim","SubModel",
             "AUCGUART","PRIMEUNIT","VNST")

mydata = mydata[!(names(mydata) %in% drop_var)]
names(mydata)

resp = "IsBadBuy"
pred = names(mydata)[names(mydata)!=resp]

mydata[,resp] = as.factor(mydata[,resp])

# sample to test code
# mydata = mydata[sample(rownames(mydata),1000,replace=FALSE),]

#### Scale: Standardize Predictors ####


var_cont = c( "VehicleAge","VehOdo",
              "MMRAcquisitionAuctionAveragePrice",
              "MMRAcquisitionAuctionCleanPrice",
              "MMRAcquisitionRetailAveragePrice",
              "MMRAcquisitonRetailCleanPrice",
              "MMRCurrentAuctionAveragePrice",
              "MMRCurrentAuctionCleanPrice",
              "MMRCurrentRetailAveragePrice",
              "MMRCurrentRetailCleanPrice","VehBCost",
              "WarrantyCost", "Differential")

mydata_pre_scale = mydata
mydata[,var_cont]= sapply(mydata[,var_cont], function(x) (x-mean(x))/sd(x)) #standardize predictors
test[,var_cont]= sapply(test[,var_cont], function(x) (x-mean(x))/sd(x)) #standardize predictors
# or use:
# scale(mydata[,var_cont])

#R commands for creating indices of partition for K-fold CV

CVInd = function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m = floor(n/K)  #approximate size of each part
  r=n-m*K  
  I=sample(n,n)  #random reordering of the indices
  Ind=list()  #will be list of indices for all K parts
  length(Ind)=K
  for (k in 1:K) {
    if (k <= r) kpart = ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart=((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] = I[kpart]  #indices for kth part of data
  }
  Ind
}


#### GINI ####


gini = function ( actual, predicted ) {
  n = length( actual );
  gini_data = as.data.frame( cbind( actual, predicted, row = 1:n ) );
  gini_data = gini_data[ with( gini_data, order( -predicted, row ) ), ];
  sum( cumsum( gini_data[ , 1 ] ) / sum( gini_data[ , 1 ] ) - ( 1:n ) / n ) / n;
}

## fit nnet

library(nnet)
out = nnet(IsBadBuy~., data = mydata, 
                  linout=F, skip=F, size=3, decay=.1, maxit=1000, trace=F)

phat = as.numeric(predict(out))
y = mydata[,resp]
yhat = as.numeric(phat >= 0.5)  #classify as 1 if predicted probability >= 0.5
sum(y != yhat)/length(y)  #misclassification rate
summary(out)
plot(phat,jitter(y,0.05))

# alternative approach
y = mydata[,resp]
yhat = predict(out,type="class")
sum(y != yhat)/length(y)
gini(as.numeric(y),phat)

## Using CV to Compare Models for the mydata Data
#R commands for creating indices of partition for K-fold CV

CVInd = function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m = floor(n/K)  #approximate size of each part
  r=n-m*K  
  I=sample(n,n)  #random reordering of the indices
  Ind=list()  #will be list of indices for all K parts
  length(Ind)=K
  for (k in 1:K) {
    if (k <= r) kpart = ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart=((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] = I[kpart]  #indices for kth part of data
  }
  Ind
}

## CV to choose the best k 

# Start the clock!
ptm <- proc.time()

Nrep = 5  #number of replicates of CV (if doing n-fold, need only 1 replicate)
n=nrow(mydata) 
K = 3  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]

models = seq(1,5,1)
lambdas = seq(.01,.1,.02)

#n iterations , 1.36 per iteraion
Nrep*length(lambdas)*length(models)*K
loops = Nrep*length(lambdas)*length(models) # without folds

#misclass= matrix(0,Nrep*length(lambdas)*length(models),4) # misclass for each model and each replicate
misclass = c()


# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  # repeat CV for each model using the same CV partition
  for (m in models){
    
    yhat = y; # reset yhat (actually don't need this)
    phat = as.numeric(y)
    
    for(l in lambdas){
      
      # CV for each model
      for (k in 1:K) {
        train = mydata[-Ind[[k]],c(resp,pred)] # does not work if you convert to 
        # matrix and then to data frame
        test = mydata[Ind[[k]],pred]
        #ytrain = mydata[-Ind[[k]],resp]
        
        out = nnet(IsBadBuy~., data =train, 
                   linout=F, skip=F, size=m, decay=l, maxit=1000, trace=F)
        
        yhat[Ind[[k]]] = predict(out, newdata = test,type="class")
        phat[Ind[[k]]] = predict(out, newdata = test, type = "raw")
        
      } #end of k loop
      
      #tab = table(y,yhat)
      #miss = 1-sum(diag(prop.table(tab)))
      #misclass[j,KNN] = miss
      misclass = rbind(misclass,c(j,m,l,sum(y!=yhat)/length(y),gini(as.numeric(y),phat)))
      
      loops = loops - 1
      print(paste("Remaining Iterations: ",loops))
      
    } # end of l loop
  }# end of m loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

colnames(misclass) = c("rep","nodes","lambda","misclass","gini")
misclass
misclassAVE = aggregate(cbind(misclass,gini)~ nodes + lambda, misclass, mean ) # aggregate reps
misclassAVE = misclassAVE[order(misclassAVE[,"nodes"]),] # sort
misclassAVE

param_opt = misclassAVE[which.min(misclassAVE[,"misclass"]),]
param_opt

write.csv(param_opt, file = "best_nnet.csv",row.names=FALSE)
write.csv(misclassAVE, file = "misclassAVE_nnet.csv",row.names=FALSE)
