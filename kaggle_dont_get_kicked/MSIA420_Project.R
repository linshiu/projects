# https://www.kaggle.com/c/DontGetKicked/forums/t/965/external-data
# https://www.census.gov/econ/geo-zip.html
# http://proximityone.com/zipequiv.htm
#### Load Data ###########################################

# Aginity
# main = "\\\\nas1/labuser169"
# course = "MSIA401_Project"
# setwd(file.path(main,course))

# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"
course = "MSIA_420_Predictive_Analytics"
datafolder = "Project"
setwd(file.path(main,course, datafolder))

#opts_knit$set(root.dir = getwd())

# Import data
filename = "training.csv"
mydata = read.csv(filename,header = T)

#### Process Data #########################################

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
ncol(mydata)
summary(mydata)

str(mydata)

# not many levels of buyers (vs number of rows), so keep this column
nlevels(factor(mydata$BYRNO))
hist(mydata$BYRNO)

# not many levels of zipcodes (vs number of rows), so keep this column
nlevels(factor(mydata$VNZIP1))

# trim levels depends on car model?
length(levels(mydata$Trim))
nlevels(mydata$Trim)
levels(mydata$Trim)

# can potentially link zipcodes to demographic-economic data

# drop first column REFID
mydata = mydata[,-which(names(mydata)=="RefId")]
ncol(mydata)

str(mydata)

resp = "IsBadBuy"

var_cont = c("VehYear", "VehicleAge","VehOdo",
             "MMRAcquisitionAuctionAveragePrice",
             "MMRAcquisitionAuctionCleanPrice",
             "MMRAcquisitionRetailAveragePrice",
             "MMRAcquisitonRetailCleanPrice",
             "MMRCurrentAuctionAveragePrice",
             "MMRCurrentAuctionCleanPrice",
             "MMRCurrentRetailAveragePrice",
             "MMRCurrentRetailCleanPrice","VehBCost",
             "WarrantyCost")

var_cat = c("IsBadBuy" , "Auction","Make", "Model",
            "Trim","SubModel","Color","Transmission",
            "WheelTypeID","WheelType","Nationality", "Size",
            "TopThreeAmericanName","PRIMEUNIT",
            "AUCGUART", "BYRNO", "VNZIP1","VNST","IsOnlineSale")

# no Kickdate?
var_date = c("PurchDate")

# convert date to numeric?

# factors and levels in R
# http://www.stat.berkeley.edu/~s133/factors.html

# convert factor to character
# http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters

# find every factor variable and convert to character (doesn't apply here)
# i = sapply(mydata, is.factor)
# mydata[i] = lapply(mydata[i], as.character)

# convert character to numeric
# http://www.statmethods.net/management/typeconversion.html

# convert levels variable to numeric (prices)
# NULL values present, when converted to numeric they will become NA
sum(as.character(mydata[,var_cont[4]])=="NULL")

# convert first to character then to numeric for continuous variables
i = sapply(mydata[,var_cont], is.factor)
mydata[,var_cont][i] = lapply(mydata[,var_cont][i], function(x) as.numeric(as.character(x)))
sum(is.na(mydata[,var_cont[4]]))

# convert numeric to factor for categorical variabes
i = sapply(mydata[,var_cat], is.numeric)
mydata[,var_cat][i] = lapply(mydata[,var_cat][i], as.factor)
# drop response variable from categorical variable list
var_cat = var_cat[-which(var_cat=="IsBadBuy")]

str(mydata)

# date format
# http://www.statmethods.net/input/dates.html
# http://www.ats.ucla.edu/stat/r/faq/string_dates.htm
# http://stackoverflow.com/questions/9216138/find-the-day-of-a-week-in-r
# http://stackoverflow.com/questions/9749598/r-obtaining-month-and-year-from-a-date
# http://www.quantlego.com/howto/introduction-dates-and-times-in-r/

# convert to dates
i = sapply(mydata[var_date], is.factor)
mydata[,var_date] = as.character(mydata[,var_date])
mydata[,var_date] = as.Date(mydata[,var_date],"%m/%d/%Y")


# create additional variables based on dates
mydata$PurchWeekDay = factor(weekdays(mydata[,var_date]),
                             levels = c("Monday", "Tuesday",
                                        "Wednesday","Thursday",
                                        "Friday", "Saturday",
                                        "Sunday"),ordered=T)

mydata$PurchMonth = factor(months(mydata[,var_date]),
                              levels=c("January","February","March",
                                       "April","May","June","July","August","September",
                                       "October","November","December"),ordered=TRUE)
                                                       
mydata$PurchQuarter = factor(quarters(mydata[,var_date]),
                             levels = c("Q1","Q2","Q3","Q4"),
                             ordered=TRUE)

mydata$PurchYear= as.numeric(format(mydata[,var_date], "%Y"))

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

############## boxplot predictors by targdol_bin 
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
