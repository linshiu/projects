# https://www.kaggle.com/c/DontGetKicked/forums/t/965/external-data
# https://www.census.gov/econ/geo-zip.html
# http://proximityone.com/zipequiv.htm
#### Load Data ###########################################

# Aginity
# main = "\\\\nas1/labuser169"
# course = "MSIA401_Project"
# setwd(file.path(main,course))

# My PC
#main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"
#course = "MSIA_420_Predictive_Analytics"
#datafolder = "Project"
#setwd(file.path(main,course, datafolder))

# SSCC
setwd("/sscc/home/a/amk202/SuddenLink/predictive")

#opts_knit$set(root.dir = getwd())

# Import data
#filename = "training.csv"
filename = "car_data.csv"
mydata = read.csv(filename,header = T)
mydata = mydata[,-which(names(mydata)=="X")]

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

# drop first column REFID
mydata = mydata[,-which(names(mydata)=="RefId")]
ncol(mydata)

str(mydata)

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

var_census = c("moved_total","moved_diff_county","moved_diff_state",
               "median_income", "mean_income", "total_population", 
               "high_school", "bachelor", "urban" , "rural" , "region")

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


str(mydata)

#### date format ####
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

mydata$PurchSeason = ""
mydata$PurchSeason[mydata$PurchMonth %in% c("December", "January","February")]="Winter"
mydata$PurchSeason[mydata$PurchMonth %in% c("March", "April","May")]="Spring"
mydata$PurchSeason[mydata$PurchMonth %in% c("June", "July","August")]="Summer"
mydata$PurchSeason[mydata$PurchMonth %in% c("September", "October","November")]="Fall"

mydata$PurchSeason = factor(mydata$PurchSeason)

#### Change Levels ####

# check levels
lapply(var_cat,function(x) levels(mydata[[x]]))

table(mydata$Transmission)
nlevels(mydata$Transmission)
levels(mydata$Transmission)

var = mydata[,"Transmission"]
current = "Manual"
newlevel = "MANUAL"
  
change_level = function (var,current,newlevel){
  levels(var)[which(levels(var) %in% current)] = newlevel
  var 
}

mydata[,"Transmission"] = change_level(var,current,newlevel)
levels(mydata[,"Transmission"] )


#### Missing Values ####

# remove instead of doing this
#nas = sapply(names(mydata), function(x) sum(is.na(mydata[,x])))
#nas_names = names(nas[nas>0])
#nas_means=sapply(nas_names,function (x) mean(mydata[,x],na.rm=TRUE))

# replace missing value by average
#for (x in nas_names){
#  mydata[is.na(mydata[,x]),x] = nas_means[x]
  
#}

# remove NA
mydata = mydata[!is.na(mydata["MMRCurrentAuctionAveragePrice"]),]
mydata = mydata[!is.na(mydata["median_income"]),]

# check number of missing
sum(sapply(names(mydata), function(x) sum(is.na(mydata[,x])))>0)

#### Price Difference (maybe innclude more) ####

mydata$DiffAcquistionAuctionPrice = mydata[,"MMRAcquisitionAuctionCleanPrice"] - mydata[,"MMRAcquisitionAuctionAveragePrice"] 
mydata$DiffAcquistionRetailPrice= mydata[, "MMRAcquisitonRetailCleanPrice"] - mydata[,"MMRAcquisitionRetailAveragePrice"]    
mydata$DiffCurrentAuctionPrice= mydata[, "MMRCurrentAuctionCleanPrice"] - mydata[,"MMRCurrentAuctionAveragePrice"] 
mydata$DiffCurrentRetailPrice= mydata[,"MMRCurrentRetailCleanPrice"] - mydata[,"MMRCurrentRetailAveragePrice" ]

# add new variables
var_cont = c(var_cont,"moved_diff_county", "moved_diff_state", "moved_total", "median_income", "mean_income" ,"total_population" ,
             "high_school", "bachelor" , "urban", "rural" , "PurchYear" ,"DiffAcquistionAuctionPrice",
             "DiffAcquistionRetailPrice",  "DiffCurrentAuctionPrice", "DiffCurrentRetailPrice" )

var_cat = c(var_cat,"region", "PurchWeekDay" , "PurchMonth", "PurchQuarter" ,
             "PurchSeason")

# Check
length(var_cont)+length(var_cat)+length(var_date)
ncol(mydata)
sort(c(var_cont,var_cat,var_date))==sort(names(mydata))

#### Scale: Standardize Predictors ####

mydata_pre_scale = mydata
mydata[,var_cont]= sapply(mydata[,var_cont], function(x) (x-mean(x))/sd(x)) #standardize predictors
# or use:
# scale(mydata[,var_cont])

#### Drop Predictors ####

mydata_pre_drop = mydata

# drop redundant and too many categories variables
# (Purchdate variables are captured in age)
drop_var = c("PurchDate","VehYear","Model", "Trim","SubModel","VNZIP1", "PurchYear","BYRNO")

mydata = mydata[!(names(mydata) %in% drop_var)]
names(mydata)

resp = "IsBadBuy"
pred = names(mydata)[names(mydata)!=resp]
