library(data.table)
library(sqldf)
library(ggplot2)
library(maps)

# Load Dataset ####
setwd("/sscc/home/l/lsl575/airline")

fileNames = list.files(path = getwd(), pattern = "*.csv")
alldata = rbindlist(lapply(fileNames, function(x) {
  xx = fread(x, header = TRUE, sep = ",", na.strings=c("NA", ''))
}))

str(alldata)
head(alldata)
dim(alldata)
names(alldata)
class(alldata)

# Transform the Dataset ####
alldata = data.frame(alldata)

drop = c("FlightNum", "TailNum", "ActualElapsedTime", "CRSElapsedTime", "AirTime", "Distance",
         "TaxiIn", "TaxiOut", "DepTime", "ArrTime")

# Drop variables
data = alldata[, -which(names(alldata) %in% drop)]

names(data)

# Subset data
dataSub=sqldf("SELECT * 
               FROM data
               WHERE (Origin='ORD' OR Origin = 'MDW') 
               AND (Dest = 'MCO' OR Dest = 'LAX')")

# Save as R dataset
save(dataSub,file="dataSub.Rda")
load("~/airline/dataSub.Rda")
data = dataSub

# Save as csv
write.csv(dataSub, file = "dataSub.csv", row.names = FALSE)

########################## Data Processing#####

#delete NA's
data[is.na(data)] = 0
mydata = data[1:100,]

#data = within(data,  dateTime = paste(Year, Month, DayofMonth, sep="-"))
data$date = paste(data$Year, data$Month, data$DayofMonth, sep= "-")
data$date = as.Date(data$date)
data$month = months(data$date)
data$dayofweek = weekdays(data$date)

str(data)

# cacellation code
# reason for cancellation
# (A = carrier, B = weather, C = NAS, D = security)

table(data$CancellationCode)
data[data[,"CancellationCode"] == "0","CancellationCode"] = "NA"
data[data[,"CancellationCode"] == "0","CancellationCode"] = "NA"
data$CancellationCode[data$CancellationCode =="0"] = "NA"
data$CancellationCode[data$CancellationCode =="A"] = "Carrier"
data$CancellationCode[data$CancellationCode =="B"] = "Weather"
data$CancellationCode[data$CancellationCode =="C"] = "NAS"

data$CancellationCode = as.factor(data$CancellationCode)
levels(data$CancellationCode)
unique(data$CancellationCode)

table(data$Cancelled, data$CancellationCode)
table(data$Dest, data$UniqueCarrier)
table(data$Origin, data$UniqueCarrier)

# Subset data
data1 = sqldf("SELECT *
               FROM data
               WHERE Dest = 'MCO' AND UniqueCarrier <> 'DL'")

str(data1)
table(data1$UniqueCarrier)

data2 =sqldf(" SELECT *, (CancelCount/TotalCount) AS pctCancel
               FROM (SELECT Year, Month,UniqueCarrier, cast(SUM(Cancelled) as real) as CancelCount, COUNT(*) as TotalCount
                     FROM data1
                     GROUP BY Year, Month,UniqueCarrier)")

table(data2$UniqueCarrier)



head(data1)
ggplot(data2, aes(x=UniqueCarrier, y=pctCancel)) + 
  geom_boxplot(aes(fill = UniqueCarrier))  +
  labs(y = "Percentage of Flighs Cancelled", x = "Airline") + 
  theme(panel.background = element_rect(fill = "#d8e7eb")) +
  guides(fill=FALSE) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18)) +
  theme(strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18))


############################### Check for later ####
mydata = data[1:100,]

# Create datetime variable
timeVars = c("Year", "Month", "DayofMonth", "DayOfWeek")


for(i in 1:nrow(mydata)){
  mydata$dateTime[i]=paste(time2$year[i], "-",time2$month[i], "-1", sep="")
}


dataSub2$dateTime = 
x = ISOdatetime(year = 2014, month = 10, day = 12, hour = 21, min = 12, sec = 0)


weekdays(x)
month(x)
strftime(x, format="%H:%M")
as.date(x)
as.Date.Time(x)

i = sapply(dataSub2[,c("Year", "DayofMonth")], function(x) class(x))
class(i)
i


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



table(dataSub$CancellationCode)


x = "2100"
y = strptime(x,"%H%M")
strftime(y, format="%H:%M")






str(dataSub)


str(dataSub)

################################### Xiang ####

setwd("/sscc/home/x/xlf278/airline")

origin_mon = read.csv("eastWestOrigins_Mon.csv",header = TRUE, na.strings=c("NA", ''))
origin_mon = origin_mon[,-c(1,11,12,21,22)]
str(origin_mon)
dele_na = origin_mon[,c(21:25)]
na = origin_mon[,-c(21:25)]
dele_na[is.na(dele_na)] <- 0
origin_mon = cbind(na,dele_na)
origin_mon = origin_mon[,-c(19)]


dest_thu = read.csv("/sscc/home/s/swr386/airline/eastWestDest_Thu.csv",header = TRUE, na.strings=c("NA", ''))
dest_thu = dest_thu[,-c(1,11,12,21,22,24)]
str(dest_thu)
dele_na = dest_thu[,c(20:24)]
na = dest_thu[,-c(20:24)]
dele_na[is.na(dele_na)] <- 0
dest_thu = cbind(na,dele_na)

save(dest_thu, file = "dest_thu.RData")
save(origin_mon, file = "origin_mon.RData")
write.csv(dest_thu,"dest_thu.csv")
write.csv(origin_mon,"origin_mon.csv")

################################### Alejandro ####
library(data.table)
library(sqldf)
library(ggplot2)
library(maps)

# Load Dataset ####
setwd("/sscc/home/l/lsl575/airline")

fileNames <- list.files(path = getwd(), pattern = "*.csv")
alldata <- rbindlist(lapply(fileNames, function(x) {
  xx <- fread(x, header = TRUE, sep = ",", na.strings=c("NA", ''))
}))

# Transform the Dataset ####

data<-sqldf("SELECT * 
            FROM alldata
            WHERE (Origin='ORD' OR Origin = 'MDW') 
            AND (Dest = 'MCO' OR Dest = 'LAX')")

head(data,20)
head(test1,20)

backup<-data
data<-backup


#delete NA's
data[is.na(data)] <- 0

dataLate15<-sqldf("SELECT *
                  FROM data 
                  WHERE ArrDelay>15")

head(dataLate15,10)

test3<-sqldf("select 
             case 
             when (CarrierDelay > WeatherDelay AND CarrierDelay > NASDelay AND CarrierDelay > SecurityDelay AND CarrierDelay > LateAircraftDelay) THEN 'CarrierDelay'
             WHEN (WeatherDelay > CarrierDelay AND WeatherDelay > NASDelay AND WeatherDelay > SecurityDelay AND WeatherDelay > LateAircraftDelay) THEN 'WeatherDelay'
             WHEN (NASDelay > CarrierDelay AND NASDelay > WeatherDelay AND NASDelay > SecurityDelay AND NASDelay > LateAircraftDelay) THEN 'NASDelay'
             WHEN (SecurityDelay > CarrierDelay AND SecurityDelay > WeatherDelay AND SecurityDelay > NASDelay AND SecurityDelay > LateAircraftDelay) THEN 'SecurityDelay'
             WHEN (LateAircraftDelay > CarrierDelay AND LateAircraftDelay > WeatherDelay AND LateAircraftDelay > SecurityDelay AND LateAircraftDelay >= NASDelay) THEN 'LateAircraftDelay' 
             ELSE 'NoDelay'
             END AS DelayReason
             FROM dataLate15")

unique(test3)

dataLate15$DelayReason<-as.matrix(test3)
dataLate15$DelayReason<-as.factor(dataLate15$DelayReason)

#take out the NoDelay ones that have problems (they should not been there because we removed the NoDelay - probably they had NA's)
update<-sqldf("SELECT *
              FROM dataLate15 
              WHERE DelayReason <> 'NoDelay'")

ggplot(data = update, aes(x = as.factor(Month), fill=factor(DelayReason))) +
  geom_bar() +
  facet_wrap(~ UniqueCarrier)

# DelayReason from Chicago to LAX ####
dataLate15CHItoLAX<-sqldf("SELECT *
                          FROM dataLate15 
                          WHERE DelayReason <> 'NoDelay'
                          AND Dest = 'LAX'")

ggplot(data = dataLate15CHItoLAX, aes(x = as.factor(Month), fill=factor(DelayReason))) +
  geom_bar() +
  facet_wrap(~ UniqueCarrier)




# DelayReason from MDW to MCO ####
dataLate15CHItoMCO<-sqldf("SELECT *
                          FROM dataLate15 
                          WHERE DelayReason <> 'NoDelay'
                          AND Dest = 'MCO'
                          AND Origin = 'MDW'")

ggplot(data = dataLate15CHItoMCO, aes(x = as.factor(Month), fill=factor(DelayReason))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  facet_wrap(~ UniqueCarrier) +
  theme(panel.background = element_rect(fill = "#d8e7eb"))




# Count of flights from Chicago to LAX and MCO ####
count<-data
count[is.na(count)] <- 0
test1<-sqldf("select 
             case 
             WHEN (CarrierDelay > WeatherDelay AND CarrierDelay > NASDelay AND CarrierDelay > SecurityDelay AND CarrierDelay > LateAircraftDelay) THEN 'CarrierDelay'
             WHEN (WeatherDelay > CarrierDelay AND WeatherDelay > NASDelay AND WeatherDelay > SecurityDelay AND WeatherDelay > LateAircraftDelay) THEN 'WeatherDelay'
             WHEN (NASDelay > CarrierDelay AND NASDelay > WeatherDelay AND NASDelay > SecurityDelay AND NASDelay > LateAircraftDelay) THEN 'NASDelay'
             WHEN (SecurityDelay > CarrierDelay AND SecurityDelay > WeatherDelay AND SecurityDelay > NASDelay AND SecurityDelay > LateAircraftDelay) THEN 'SecurityDelay'
             WHEN (LateAircraftDelay > CarrierDelay AND LateAircraftDelay > WeatherDelay AND LateAircraftDelay > SecurityDelay AND LateAircraftDelay >= NASDelay) THEN 'LateAircraftDelay' 
             ELSE 'NoDelay'
             END AS DelayReason
             FROM count")

count$DelayReason<-as.matrix(test1)
count$DelayReason<-as.factor(count$DelayReason)

test2<-sqldf("select 
             case 
             WHEN (ArrDelay>15) THEN 1
             ELSE 0
             END AS DummyDelay
             FROM count")

count$DummyDelay<-as.matrix(test2)
count$DummyDelay<-as.numeric(count$DummyDelay)



unique(update2$DummyDelay)


ggplot(data = count, aes(x = as.factor(Dest), fill=factor(DummyDelay))) +
  geom_bar(position = "dodge")

# Time series for delay in every airport ####

#By day
timeSeries<-data.frame(data)

test5<-sqldf("select 
             case 
             WHEN (ArrDelay>15) THEN 1
             ELSE 0
             END AS DummyDelay
             FROM timeSeries")

timeSeries$DummyDelay<-as.matrix(test5)
timeSeries$DummyDelay<-as.numeric(timeSeries$DummyDelay)


time<-sqldf("select *, (delay/total) as percenDelayed 
            from (
            select Origin, Dest, year, month, DayofMonth, count(*) as total, sum(DummyDelay) as delay 
            from timeSeries 
            group by origin, dest, year, month, DayofMonth
            )")

unique(time$year)

for(i in 1:nrow(time)){
  time$date[i]<-paste(time$year[i], "-",time$month[i], "-", time$DayofMonth[i], sep="")
}
time$date<-as.Date(time$date)

ggplot(data=time, aes(x=date, y=percenDelayed, colour=factor(Dest))) + 
  geom_line() + 
  stat_smooth()

#too croweded, so try by month

timeSeries2<-data.frame(data)

test6<-sqldf("select 
             case 
             WHEN (ArrDelay>15) THEN 1
             ELSE 0
             END AS DummyDelay
             FROM timeSeries2")

timeSeries2$DummyDelay<-as.matrix(test6)
timeSeries2$DummyDelay<-as.numeric(timeSeries2$DummyDelay)


time2<-sqldf("select *, (delay/total) as percenDelayed 
             from (
             select Origin, Dest, year, month, count(*) as total, sum(DummyDelay) as delay 
             from timeSeries2 
             group by origin, dest, year, month
             )")

unique(time2$year)

for(i in 1:nrow(time2)){
  time2$date[i]<-paste(time2$year[i], "-",time2$month[i], "-1", sep="")
}
time2$date<-as.Date(time2$date)

ggplot(data=time2, aes(x=date, y=percenDelayed, colour=factor(Dest))) + 
  geom_line() + 
  stat_smooth()



#Separating for every route

for(i in 1:nrow(time2)){
  time2$route[i]<-paste("From: ",time2$Origin[i], "   To: ",time2$Dest[i], sep="")
}
time2$route<-as.factor(time2$route)

ggplot(data=time2, aes(x=date, y=percenDelayed, colour=factor(route))) + 
  geom_line() + 
  stat_smooth() +
  ylim(0,0.5)

ggplot(data=time2, aes(x=date, y=percenDelayed, colour=factor(route))) +  
  stat_smooth(se=F, size=2) +
  ylim(0,0.4) +
  labs(y = "Percentage of Flighs Delayed",
       x = "Time") + 
  theme(panel.background = element_rect(fill = "#d8e7eb")) +
  theme(legend.key = element_rect(fill = "#d8e7eb")) +
  scale_colour_manual(values = c("#c163c1","#800080","#FFA500","#FF4040"),
                      breaks = c("From: ORD   To: LAX", "From: ORD   To: MCO", 
                                 "From: MDW   To: LAX", "From: MDW   To: MCO"),
                      name = "Flight Routes") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18))




# Maps ####

#load us map data
all_states <- map_data("state")
#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p

mydata <- read.csv("/sscc/home/l/lsl575/airline/geo/geo.csv", header=TRUE, row.names=1, sep=",")

mapData1<-data
mapData2<-sqldf("select Origin, Dest, count(*) AS count
                from mapData1
                where (Origin = 'ORD' AND Dest = 'MCO')
                OR (Origin = 'ORD' AND Dest = 'LAX')
                OR (Origin = 'MDW' AND Dest = 'MCO')
                OR (Origin = 'MDW' AND Dest = 'LAX')
                Group by Origin, Dest
                Order by Dest")


#plot all the states and the data

p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white" )

p <- p + geom_point( data=mydata, aes(x=long, y=lat, size = (NumFlights)*5), color="coral1") + scale_size(name="Number of Flights")
p <- p + geom_text( data=mydata, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=label), colour="gold2", size=6 )
p

# for individual states 

CA <- subset(all_states, region %in% 'california' )

p <- ggplot()
p <- p + geom_polygon( data=CA, aes(x=long, y=lat, group = group),colour="white" )

p <- p + geom_point( data=mydata, aes(x=long[4], y=lat[4], size = enrollment[4]), color="coral1") + scale_size(name="Total enrollment")
p1 <- p + geom_text( data=mydata, hjust=0.5, vjust=-0.5, aes(x=long[4], y=lat[4], label=label[4]), colour="gold2", size=4 )
p1


FL <- subset(all_states, region %in% 'florida' )

p <- ggplot()
p <- p + geom_polygon( data=FL, aes(x=long, y=lat, group = group),colour="white" )

p <- p + geom_point( data=mydata, aes(x=long[1], y=lat[1], size = enrollment[1]), color="coral1") + scale_size(name="Total enrollment")
p2 <- p + geom_text( data=mydata, hjust=0.5, vjust=-0.5, aes(x=long[1], y=lat[1], label=label[1]), colour="gold2", size=4 )
p2

IL <- subset(all_states, region %in% 'illinois' )

p <- ggplot()
p <- p + geom_polygon( data=IL, aes(x=long, y=lat, group = group),colour="white" )

p <- p + geom_point( data=mydata, aes(x=long[2:3], y=lat[2:3], size = enrollment[2:3]), color="coral1") + scale_size(name="Total enrollment")
p3 <- p + geom_text( data=mydata, hjust=0.5, vjust=-0.5, aes(x=long[2:3], y=lat[2:3], label=label[2:3]), colour="gold2", size=4 )
p3

# Distribution of ArrDelay in both MCO and LAX ####

distribution <- sqldf("select * 
                      from data 
                      where Dest = 'MCO' OR Dest='LAX'") #same as the data

distribution <- data
test8<-sqldf("select 
             case 
             WHEN (ArrDelay>15) THEN 1
             WHEN (ArrDelay<0) THEN 2
             ELSE 0
             END AS DummyDelay
             FROM distribution")

distribution$DummyDelay<-as.matrix(test8)
distribution$DummyDelay<-as.numeric(distribution$DummyDelay)


ggplot(distribution, aes(x=ArrDelay, fill = factor(DummyDelay))) + 
  geom_histogram(aes(y=..count../sum(..count..)),binwidth = 1) +
  geom_vline(xintercept = 15, colour="black", linetype = "longdash") +
  geom_vline(xintercept = 0, colour="red") +
  facet_grid(Origin ~ Dest) +
  xlim(-50,250) +
  theme(panel.background = element_rect(fill = "#d8e7eb")) +
  scale_fill_manual(values=c("#408740", "#df2c2c", "#56B4E9"), 
                    name="Legend",
                    breaks=c("0", "1", "2"),
                    labels=c("On-time", "Delayed Arrival", "Early Arrival")) +
  labs(y = "Percentage of counts",
       x = "Time (in minutes)") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18)) +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18))





ggplot(distribution, aes(x=ArrDelay, fill = factor(DummyDelay))) + 
  geom_density() +
  geom_vline(xintercept = 15, colour="green", linetype = "longdash") +
  geom_vline(xintercept = 0, colour="red") +
  facet_grid(Origin ~ Dest) +
  xlim(-100,250)

#preserving marginal densities (aka count)
#ggplot(distribution, aes(x=ArrDelay, y=..count.., fill = factor(DummyDelay))) + 
ggplot(distribution, aes(x=ArrDelay, y=..scale.., fill = factor(DummyDelay))) +   
  geom_density() +
  geom_vline(xintercept = 15, colour="green", linetype = "longdash") +
  geom_vline(xintercept = 0, colour="red") +
  facet_grid(Origin ~ Dest) +
  xlim(-100,250)

#check<-sqldf("select * from distribution where ArrDelay >= 250")


# Distribution of ArrDelay in LAX ####
distributionLAX<-sqldf("select *
                       from distribution
                       where Dest = 'LAX'")

ggplot(distributionLAX, aes(x=ArrDelay)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 15, colour="green", linetype = "longdash") 


# Distribution of ArrDelay in MCO ####
distributionMCO<-sqldf("select *
                       from distribution
                       where Dest = 'MCO'")

ggplot(distributionMCO, aes(x=ArrDelay, fill = factor(DummyDelay))) + 
  geom_histogram(binwidth = 3) +
  geom_vline(xintercept = 15, colour="green", linetype = "longdash") +
  xlim(-50,300)



# Calendar ####

#From Midway to MCO
calendar<-distribution
test9<-sqldf("select 
             case 
             WHEN (ArrDelay>15) THEN 1
             ELSE 0
             END AS DummyDelay
             FROM calendar")

calendar$DummyDelay<-as.matrix(test9)
calendar$DummyDelay<-as.numeric(calendar$DummyDelay)

totalDelay<-sqldf("select count(*) AS total 
                  from calendar
                  where Origin = 'MDW' AND Dest = 'MCO' 
                  AND Year>= 2003
                  group by Month, DayOfWeek")

calendarDelay<-sqldf("select Month, DayOfWeek, count(DummyDelay) AS CountDelay 
                     from calendar
                     where Origin = 'MDW' AND Dest = 'MCO' 
                     AND Year>= 2003 
                     AND DummyDelay=1
                     group by Month, DayOfWeek")

calendarDelay$Total<-as.matrix(totalDelay)
calendarDelay$Total<-as.numeric(calendarDelay$Total)

percDelay<-sqldf("select (CountDelay/Total) AS percDelay
                 from calendarDelay")

calendarDelay$percDelay<-as.matrix(percDelay)
calendarDelay$percDelay<-as.numeric(calendarDelay$percDelay)

ggplot(calendarDelay, aes(factor(Month), DayOfWeek)) + 
  geom_tile(aes(fill = percDelay), colour = "white") + 
  scale_fill_gradient(low = "#F7FBFF", high = "#08306B", name = "Percentage of\nDelayed Flights") +
  labs(x = "Month",
       y = "Day of the Week") + 
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                   labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) + 
  scale_y_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7"),
                   labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size = 18, angle=90, vjust=0.5),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size = 18)) +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))




