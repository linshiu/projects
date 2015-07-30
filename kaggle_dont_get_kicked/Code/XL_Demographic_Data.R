setwd("/sscc/home/a/amk202/SuddenLink/_predictive")
geo_mobility = read.csv("/sscc/home/x/xlf278/predictive project/ACS_13_5YR_B07010_with_ann.csv");
income = read.csv("/sscc/home/x/xlf278/predictive project/ACS_13_5YR_S1901_with_ann.csv");
population = read.csv("/sscc/home/x/xlf278/predictive project/DEC_10_SF2_PCT1_with_ann.csv",header=T);
edu_level = read.csv("/sscc/home/x/xlf278/predictive project/ACS_13_5YR_S1501_with_ann.csv",header=T);
rural_urban = read.csv("/sscc/home/x/xlf278/predictive project/DEC_10_SF1_H2_with_ann.csv",header=T);
training = read.csv("/sscc/home/x/xlf278/predictive project/training.csv",header=T);
zip_map = read.csv("/sscc/home/x/xlf278/predictive project/Zip_to_ZCTA_Crosswalk_JSI2014.csv",header=T);

library(sqldf)
data = sqldf("select * from training as a inner join zip_map as b on a.VNZIP1 = b.ZIP")
data = sqldf("select * from data as a left join geo_mobility as b on a.ZCTA_crosswalk = b.Id")
data = sqldf("select * from data as a left join income as b on a.ZCTA_crosswalk = b.Id")
data=data[,-which(names(data)==c("Id"))]
data = sqldf("select * from data as a left join population as b on a.ZCTA_crosswalk = b.Id")
data = sqldf("select * from data as a left join edu_level as b on a.ZCTA_crosswalk = b.Id")
data=data[,-which(names(data)==c("Id"))]
data = sqldf("select * from data as a left join rural_urban as b on a.ZCTA_crosswalk = b.Id")
data=data[,-which(names(data) %in% c("Id","ZCTA_crosswalk","ZIP_TYPE", "ZIP","PO_NAME","STATE"))]
colnames(data)[which(names(data)==c("Estimate..Total."))] = "moved_total"
colnames(data)[which(names(data)==c("Estimate..Total....Moved.from.different.county.within.same.state."))] = "moved_diff_county"
colnames(data)[which(names(data)==c("Estimate..Total....Moved.from.different.state."))] = "moved_diff_state"
colnames(data)[which(names(data)==c("Households..Estimate..Median.income..dollars."))] = "median_income"
colnames(data)[which(names(data)==c("Households..Estimate..Mean.income..dollars."))] = "mean_income"
colnames(data)[which(names(data)==c("Total"))] = "total_population"
colnames(data)[which(names(data)==c("Total..Estimate..Percent.high.school.graduate.or.higher"))] = "high_school"
colnames(data)[which(names(data)==c("Total..Estimate..Percent.bachelor.s.degree.or.higher"))] = "bachelor"
colnames(data)[which(names(data)==c("Urban."))] = "urban"
colnames(data)[which(names(data)==c("Rural"))] = "rural"


data$region = "";
west = c("WA", "OR", "MT", "ID", "WY", "CA", "NV", "UT", "CO", "NM", "AZ");
midwest = c("ND", "SD", "NE", "KS","MN","IA","MO","WI","IL","IN","MI","OH");
south = c("OK", "TX", "AR", "LA", "MS", "AL", "TN", "KY", "GA", 
          "FL", "SC", "NC", "VA", "WV", "DC", "MD", "DE")
northwest = c("PA", "NY", "NJ", "CT", "MA", "VT", "NH", "ME", "RI")

car = data
car$VNST = as.character(car$VNST)
car$region[(car$VNST %in% west)] = "West"
car$region[(car$VNST %in% midwest)] = "midwest"
car$region[(car$VNST %in% south)] = "south"
car$region[(car$VNST %in% northwest)] = "northwest"

change_fac = c("median_income","mean_income","high_school","bachelor")
car[,change_fac] = sapply(car[,change_fac], function(x) as.numeric(as.character(x)))


car[,"high_school"] = car[,"high_school"]/100
car[,"bachelor"] = car[,"bachelor"]/100

write.csv(car,"car_data.csv")
