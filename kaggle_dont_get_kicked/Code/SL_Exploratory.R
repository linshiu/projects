# SSCC
setwd("/sscc/home/a/amk202/SuddenLink/predictive")

#### Exploratory: categorical ##############################
# Add names to table in R
# http://stackoverflow.com/questions/19963960/adding-extra-column-name-and-row-name-to-a-table-in-r

# Visualization contingency tables
# http://statmath.wu.ac.at/projects/vcd/
# http://www.statmethods.net/advgraphs/mosaic.html
# http://www.datavis.ca/online/mosaics/about.html

library(vcd)

# too many levels to print
noprint = c("Model","Trim","SubModel")

crosstabs_list =list()

for (var in var_cat){
  mytable = table(mydata[,var],mydata[,resp])
  names(dimnames(mytable))=c(var,resp)
  
  crosstabs_list[[var]]= mytable
  
  mytable2 = cbind(prop.table(mytable, 1),mytable)
  names(dimnames(mytable2))=c(var,resp)
  
  if(!(var %in% noprint)){
    print(mytable2)
    
    save_name = paste(c("mosaic_",var,".jpg"),sep="",collapse="")
    jpeg(save_name)
    mosaic(mytable,shade=TRUE,legend=TRUE)
    dev.off()
  }
  save_name = paste(c("crosstab_",var,".csv"),sep="",collapse="")
  write.csv(mytable2, save_name)
  
}

fit = glm( IsBadBuy ~ . - Model - Trim - SubModel , family=binomial(link="logit"), data=mydata)

####### Univariate stats (e.g. mean, skewness, frequency)

# http://en.wikibooks.org/wiki/R_Programming/Descriptive_Statistics
# http://www.statmethods.net/stats/descriptives.html

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

stats_cont_byIsBadBuy = describeBy(mydata[,var_cont], group= mydata[,"IsBadBuy"])
stats_cont_byIsBadBuy

### Dates
summary(mydata[,var_date])
write.csv(summary(mydata[,var_date]), "stats_cat.csv")


### Categorical

#install.packages("descr")
library("descr")

freq(mydata[,resp])

for (var in var_cat){
  print(var)
  save_name = paste(c("freq_",var,".csv"),sep="",collapse="")
  write.csv(freq(mydata[,var],main=var), save_name)
  
}


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
mydata2 = subset(mydata,mydata$IsBadBuy!=0) # remove zeros
dim(mydata2)

plotHistBox = function(variable,xl,xu,title){
  nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
  par(mar=c(3.1, 3.1, 1.1, 2.1))
  hist(variable,xlim=c(xl,xu), col = "lightblue", main=title)
  boxplot(variable, horizontal=TRUE,  outline=TRUE,ylim=c(xl,xu), frame=F, col = "lightblue", width = 10)
  
  
}


for (i in 1:length(var_cont)){
  variable = mydata[,var_cont[i]]
  xl = min(variable,na.rm = TRUE)
  xu = max(variable,na.rm = TRUE)
  title = paste(c("histogram of ",var_cont[i]),sep="",collapse="")
  save_name = paste(c(var_cont[i],".jpg"),sep="",collapse="")
  jpeg(save_name)
  plotHistBox(variable,xl,xu,title)
  dev.off()}



###### 

# Now used the clean data with new variable for the following analysis


############## Correlation plot + matrix

numeric_var = var_cont
corr = round(cor(mydata[,numeric_var ]),2)
corr

library(corrplot)
corrplot.mixed(corr, upper = "ellipse", lower = "number")
# corrplot function changes margin, so next graph looks messed up. change to default
par(mar=c(5,4,4,2)+0.1)


pairs(corr)


############## boxplot predictors by response 
library(ggplot2)
mydata2=mydata
mydata2$IsBadBuy =as.factor(mydata$IsBadBuy)
mydata2$color[mydata$IsBadBuy==0]="red"
mydata2$color[mydata$IsBadBuy==1]="blue"

ggplot(mydata2, aes(x=IsBadBuy, y=VehYear, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=VehicleAge, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=VehOdo, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=MMRAcquisitionAuctionAveragePrice, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=MMRAcquisitionAuctionCleanPrice, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=MMRAcquisitionRetailAveragePrice, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=MMRAcquisitonRetailCleanPrice, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=MMRCurrentRetailAveragePrice, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=MMRCurrentRetailCleanPrice, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=VehBCost, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
ggplot(mydata2, aes(x=IsBadBuy, y=WarrantyCost, fill=IsBadBuy)) + geom_boxplot() + guides(fill=FALSE)
