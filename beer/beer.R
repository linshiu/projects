##### Load data ##############################################################

# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_411_Data_Visualization"
datafolder = "/Project2"
setwd(file.path(main,course, datafolder))

#carsdata =read.csv("04cars data.csv",header=TRUE,
#                   na.strings=c("","*","NA"))

# SSCC
# setwd("/sscc/home/l/lsl575/")
filename = 'BEERZ.csv'

library(data.table)
library(sqldf)
library(ggplot2)
library(plyr)

alldata = fread(filename, header = TRUE, sep = ",", na.strings=c("NA", ''))
alldata = data.frame(alldata)
#alldata = alldata[,c(-1)] # remove first column (row number)
save(alldata,file="alldata.Rda")
#load("alldata.Rda")
alldata = data.frame(alldata)

### Data Processing #########################################################

# Look at data
names(alldata)
head(alldata)
nrow(alldata)
summary(alldata)
str(alldata)

data = alldata

# # Subset data
# dataSub=sqldf("SELECT * 
#                FROM data
#                WHERE (Origin='ORD' OR Origin = 'MDW') 
#                AND (Dest = 'MCO' OR Dest = 'LAX')")

### Convert to date format
#i = sapply(mydata[var_date], is.factor)
#mydata[,var_date] = as.character(mydata[,var_date])

var_date = c("date_add")
data[,var_date ] = as.Date(data[,var_date ],"%m-%d-%Y")
str(data)

var_date = c("Date")
data[,var_date ] = as.Date(data[,var_date ],"%b %d, %Y")
str(data)

### Convert alcohol to number
var = c("alcohol")
data[,var] = gsub(" ","",data[,var]) # remove leading and trailing whitespace
data[,var] = gsub("%","",data[,var]) # remove %
data[,var] = as.numeric(data[,var])
str(data)

### Consolidate styles
load("Styles_data.rda")
head(data$style)
data$style = substr(data$style,2,nchar(data$style)-1) # remove ' at beginning and trailing space
head(data$style)

#  distinct styles
distinctSyle = sqldf("SELECT style, count(*)
                      FROM data
                      GROUP BY style
                      ORDER BY count(*)
                      ")

data = sqldf("SELECT A.*, B.Style_Group
               FROM data A
                LEFT JOIN Styles_data B ON A.style = B.style                     
              ")

var = c("Style_Group")
na = is.na(data[,var])
data[na,var] = "Other"

#  distinct styles group
distinctSyleG = sqldf("SELECT Style_Group, count(*)
                      FROM data
                      GROUP BY Style_Group
                      ORDER BY count(*)
                      ")

data$Style_Group = as.factor(data$Style_Group)
data$Style_Group = relevel(data$Style_Group, "Other")
levels(data$Style_Group)
table(data$Style_Group)

### Clean brewery_name

head(data$brewery_name)
var = c("brewery_name")
data[,var] = gsub("<b>","",data[,var]) # remove remove <b>
data[,var] = gsub(">","",data[,var]) # remove remove >
data[,var] = gsub("\"","",data[,var]) # remove "remove "

distinctBreweries = sqldf(" SELECT brewery_name, count(*)
                            FROM data
                            Group By brewery_name
                            Order By count(*)
                          ")

### Clean Location
table(data$brewery_loc)
var = c("brewery_loc")
data$country = data$brewery_loc

US = substr(data$brewery_loc,0, 1) == "9" & !is.na(data$brewery_loc)
data[US,"country"] = "US"

Na = is.na(data$brewery_loc)
data[Na,"country"] = "Other"

data[,"country"] = gsub(" ","",data[,"country"]) # remove whitespace
sub = data[(!US & !Na),"country"] 
data[(!US & !Na),"country"] = substr(sub,nchar(sub)-2,nchar(sub)-1)

table(data$country)

data$state = data$country
data[US,"state"] = data[US,"brewery_loc"]
data[US,"state"] = gsub(" ","",data[US,"state"]) # remove whitespace
sub = data[US,"state"] 
data[US,"state"] = substr(sub,nchar(sub)-2,nchar(sub)-1)
data[!US,"state"] = "NA"

table(data$state)

# Save as R dataset
alldata_clean = data
save(alldata_clean,file="alldata_clean.Rda")

# Save as csv
write.csv(alldata, file = "alldata_clean.csv", row.names = FALSE)

# Subset ####
drop = c("Text","Username", "V1", "brewery_loc")

# Drop variables
data = data[, -which(names(data) %in% drop)]

save(data,file="data.Rda")
write.csv(data, file = "data.csv", row.names = FALSE)

### Regression #########################################################
fit = lm(Overall ~ Look + Smell + Taste + Feel, data= data)
summary(fit)
#plot(fit)

fit = lm(Overall ~ Look + Smell + Taste + Feel + country, data= data)
summary(fit)

fit = lm(Overall ~ Look + Smell + Taste + Feel + Style_Group, data= data)
summary(fit)

fit = lm(Overall ~ Look + Smell + Taste + Feel + alcohol, data= data)
summary(fit)

fit = lm(Overall ~ Look + Smell + Taste + Feel + alcohol +
           Look*alcohol + Smell*alcohol + Taste*alcohol + Feel*alcohol +
           Look*Smell + Look*Taste + Look*Feel +
           Smell*Taste + Smell*Taste + Taste *Feel
         , data= data)
summary(fit)

step(fit)

### T-test #########################################################

# Find the mean of each group
cdat = ddply(sub, "Style_Group", summarise, Overall.mean=mean(Overall))
cdat

t.test(data[data$Style_Group == "American Ales","Overall"], 
data[data$Style_Group == "English Ales","Overall"])

# Overlaid histograms
ales = data$Style_Group == "American Ales" | data$Style_Group == "English Ales"
sub = data[ales,]

# http://www.sthda.com/english/articles/print/9-ggplot2-histogram-easy-histogram-with-r-ggplot2-package/
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# http://www.sthda.com/english/wiki/print.php?id=187
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

ggplot(sub, aes(x=Overall, fill=Style_Group)) + 
  geom_histogram(position="identity", alpha=0.5, binwidth = 0.5)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text= element_text(size=18),
        legend.title = element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'black')
        )+
  xlab("Overall Rating") +
  ylab("Frequency") +
  geom_vline(data=cdat, aes(xintercept=Overall.mean,  colour=Style_Group),
             linetype="dashed", size=1) +
  geom_text(data=cdat, mapping=aes(x=Overall.mean, 
                                   y=0, label=round(Overall.mean,2)), 
            size=5, angle=90, vjust=-0.4, hjust=-0.5) +
  scale_color_manual(values=c("gray12", "chocolate2"))+
  scale_fill_manual(values=c("gray12", "chocolate2"))


### T-test #########################################################

# Differences in mean Bro Score and Rest

