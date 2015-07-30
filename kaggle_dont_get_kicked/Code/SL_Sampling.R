z# Features creation
# Price differences

# Missing : continuous = average
#           categorical = missing category


# Unbalance data, down sample/ over sample ?

# standardize variables

# precision + recall -> auc

# change response variable name to y
colnames(mydata)[names(mydata)==resp]="y"
names(mydata)
resp = "y"

t = table(mydata[resp])
prop.table(t)

mydata_pre_balance = mydata

current_ratio = sum(mydata[resp]==1)/sum(mydata[resp]==0)
new_ratio = 0.5
n0 = sum(mydata[resp]==0)
n1 = sum(mydata[resp]==1)
n1/new_ratio

s0 = sample(rownames(mydata[mydata[resp]==0,]),n0*new_ratio ,replace=FALSE)
balanced = rbind(mydata[mydata[resp]==1,],mydata[s0,])
table(balanced$y)

sam.ctrl<-dat[sample(row.names(dat[dat$case==0],n.ctrl*sam.rate,replace=F),] 
rbind(dat[dat$case==1,],sam.ctrl)

library (randomForest)
rF =randomForest(y~.,data=mydata ,subset =mydata ,
                        mtry=13, importance =TRUE)
my.RF = randomForest(y~.,data=mydata)

install.packages("ROSE")
library(ROSE)
mydata <- mydata[sample(1:nrow(mydata), 50,
                          replace=FALSE),]

data.balanced.over = ovun.sample(IsBadBuy~., data=mydata,p=0.5, seed=1,method="over")$data
table(data.balanced.over$IsBadBuy)

sample(mydata,0.01)




mydata[c(rownames(mydata[mydata$y==1,]), sample(rownames(mydata[dfrm$y==0]), 0.10)) , ]


sink("sink-examp.txt")
i <- 1:10
outer(i, i, "*")
sink()
unlink("sink-examp.txt")
