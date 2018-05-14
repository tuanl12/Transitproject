#should install all of these packages
install.packages("ggmap")
install.packages("ggplot2")
install.packages("ggproto")
install.packages("chron")
install.packages("scales")
install.packages(c("dbplyr", "RSQLite"))

#load all of this libraries
library(ggmap)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(dbplyr)
library(reshape)
library(reshape2)
setwd("~/Dropbox/proj/fta/")

#From here to line 58 - read all the data files input of ventra and combine them into 1 giant data frames with 88 columns
d_ventra1 = read_delim("/Users/tuanle/20160301_20160331_vp080_ncs_tran_extract_0001.dat", delim = "|",quote = '"', col_names=F, col_types = cols(.default = "c"))
d_ventra2 = read_delim("/Users/tuanle/20160301_20160331_vp080_ncs_tran_extract_0002.dat", delim = "|",quote = '"', col_names=F,col_types = cols(.default = "c"))
d_ventra3 = read_delim("/Users/tuanle/20160301_20160331_vp080_ncs_tran_extract_0003.dat", delim = "|",quote = '"', col_names=F, col_types = cols(.default = "c"))
d_ventra4 = read_delim("/Users/tuanle/vp080_oct_2015_ncs_extract_0001.dat", delim = "|",quote = '"', col_names=F, col_types = cols(.default = "c"))
d_ventra5 = read_delim("/Users/tuanle/vp080_oct_2015_ncs_extract_0002.dat", delim = "|",quote = '"', col_names=F, col_types = cols(.default = "c"))
d_ventra6 = read_delim("/Users/tuanle/vp080_oct_2015_ncs_extract_0003.dat", delim = "|",quote = '"', col_names=F, col_types = cols(.default = "c"))

d_ventra_abp1 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0001.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp2 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0002.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp3 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0003.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp4 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0004.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp5 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0005.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp6 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0006.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp7 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0007.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp8 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0008.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp9 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0009.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp10 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0010.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp11 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0011.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp12 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0012.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp13 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0013.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp14 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0014.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp15 = read_delim("/Users/tuanle/20160301_20160331_vp080_abp_tran_extract_0015.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp16 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0001.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp17 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0002.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp18 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0003.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp19 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0004.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp20 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0005.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp21 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0006.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp22 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0007.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp23 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0008.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp24 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0009.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp25 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0010.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp26 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0011.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp27 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0012.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp28 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0013.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
d_ventra_abp29 = read_delim("/Users/tuanle/vp080_oct_2015_abp_extract_0014.dat", delim = "|", quote = '"', col_names =F, col_types = cols(.default = "c"))
                            
total_list <- rbind(d_ventra1,d_ventra2,d_ventra3, d_ventra4, d_ventra5, d_ventra6, d_ventra_abp1[,1:88], d_ventra_abp2[,1:88], 
                    d_ventra_abp3[,1:88],d_ventra_abp4[,1:88],d_ventra_abp5[,1:88],d_ventra_abp6[,1:88],
                    d_ventra_abp7[,1:88],d_ventra_abp8[,1:88],d_ventra_abp9[,1:88],d_ventra_abp10[,1:88],
                    d_ventra_abp11[,1:88],d_ventra_abp12[,1:88],d_ventra_abp13[,1:88],d_ventra_abp14[,1:88],
                    d_ventra_abp15[,1:88], d_ventra_abp16[,1:88],d_ventra_abp17[,1:88],d_ventra_abp18[,1:88],
                    d_ventra_abp19[,1:88],d_ventra_abp20[,1:88],d_ventra_abp21[,1:88], d_ventra_abp22[,1:88], 
                    d_ventra_abp23[,1:88], d_ventra_abp24[,1:88], d_ventra_abp25[,1:88], d_ventra_abp26[,1:88], 
                    d_ventra_abp27[,1:88], d_ventra_abp28[,1:88], d_ventra_abp29[,1:88])
total_list <- total_list[, colSums(is.na(total_list)) != nrow(total_list)]
total_list <- total_list[, colSums(is.na(total_list)) == 0]
#eliminate all cash transactions out of the dataset
total_list2 <- total_list[!(total_list$X33 == "No Payment"),]

#rename the column headers for easy interpretation and data manipulation
d1 = dplyr::rename(total_list, lat="X3", lon="X4", DW_TRANSACTION_ID=X1, DEVICE_ID=X2, BENEFIT_VALUE=X5, TRANSACTION_DTM=X64, PACE_RUN_ID=X20, DIRECTION=X58, START_TIME=X61, TRIP_TYPE=X71, Hour=X24, Count=X19,ROUTE_Number=X51, COST=X26, Facility=X12, Trans_status=X33, OPERATOR=X47, STOP=X50, PREV_LATITUDE=X76, PREV_LONGITUDE=X77, CARD_TYPE=X46)
#create a column that denotes whether a transaction is paid by cash or not
d1$CASH_YESNO <- ifelse(d1$Trans_status == "No Payment", "YES", "NO")
#rename column header of data frame total_list2
d1_new = dplyr::rename(total_list2, lat=X3, lon=X4, DW_TRANSACTION_ID=X1, DEVICE_ID=X2, BENEFIT_VALUE=X5, TRANSACTION_DTM=X64, PACE_RUN_ID=X20, DIRECTION=X58, START_TIME=X61, TRIP_TYPE=X71, Hour=X24, Count=X19,ROUTE_Number=X51, COST=X26, Facility=X12, Trans_status=X33, OPERATOR=X47, STOP=X49, PREV_LATITUDE=X76, PREV_LONGITUDE=X77, CARD_TYPE=X46)
#select a subset of d1 with only columns essential to our EDA defined as in col_title
col_title <- c("lat", "lon", "START_TIME", "DIRECTION", 
          "Hour", "TRIP_TYPE", "STOP", "COST", "Count", "ROUTE_Number", "Trans_status")
d1 =  d1[col_title]
d1_new = d1_new[col_title]
#obtain the hour of each trip
d1$Hour <- substr(d1$Hour,1,2) 
d1_new$Hour <- substr(d1_new$Hour,1,2)
#obtain the start time in hour of each trip
d1$START_TIME <- substr(d1$START_TIME,6,10)
d1_new$START_TIME <- substr(d1_new$START_TIME,6, 10)

#convert lat-lon into numeric type
d1 = mutate(d1, lat = as.numeric(lat))
d1_new = mutate(d1_new, lat = as.numeric(lat))
d1 = mutate(d1, lon = as.numeric(lon))
d1_new = mutate(d1_new, lon = as.numeric(lon))
#divide them by 1e6 to convert into decimal numbers
d1$lat <- as.numeric(d1$lat)/1e6
d1_new$lat <- as.numeric(d1_new$lat)/1e6
d1$lon <- as.numeric(d1$lon)/1e6
d1_new$lon <- as.numeric(d1_new$lon)/1e6

#eliminate all the rows with missing lat-lon (both "NA")
d1 <- d1[!(is.na(d1$lat)) & !(is.na(d1$lon)),]
#narrow down the dataset into only those with lat >20 and lon <-86 in order to plot the area concentrated the most number of bus stops
d1 = d1 %>% filter(lat > 20, lon < - 86)
d1_new = d1_new %>% filter(lat>41, lon<-85)

require(ggmap)
#visualize the locations of our stops on google map
stops = d1 %>% select(lat,lon) %>% distinct()
sbbox <- make_bbox(lon = as.numeric(stops$lon), lat = as.numeric(stops$lat), f = .05)
sq_map <- get_map(location = sbbox, source = "google")
ggmap(sq_map) + geom_point(data = stops, mapping = aes(x = lon, y = lat, color = "red"))
region <- get_map(location = c(min(d1$lon),
                              min(d1$lat),
                              max(d1$lon),
                              max(d1$lat)),
                 source = "google")

stops2 = d1_new %>% select(lat,lon) %>% distinct()
sbbox2 <- make_bbox(lon = as.numeric(stops2$lon), lat = as.numeric(stops2$lat), f = .05)
sq_map2 <- get_map(location = sbbox2, source = "google")
ggmap(sq_map2) + geom_point(data = stops2, mapping = aes(x = lon, y = lat, color = "red"))

region <- get_map(location = c(min(d1_new$lon),
                               min(d1_new$lat),
                               max(d1_new$lon),
                               max(d1_new$lat)),
                  source = "google")

ggplot() + 
  coord_cartesian() +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_hue() +
  layer(
    data= stops, 
    mapping=aes(x=lon, y= lat, color= "red"), 
    stat="identity", 
    geom="point", 
    position=position_jitter()
  )
#convert all char type to numeric type for Cout, Hour, Trip Type, STOP, Cost, and Route number
d1$Count <- as.numeric(d1$Count)
d1$Hour <- as.numeric(d1$Hour)
d1$TRIP_TYPE <- as.numeric(d1$TRIP_TYPE)
d1$STOP <- as.numeric(d1$STOP)
d1$COST <- as.numeric(d1$COST)
d1$ROUTE_Number <- as.numeric(d1$ROUTE_Number)

d2 <- d1 %>% select(lat, lon, DW_TRANSACTION_ID)
#d1 %>% select(Facility) %>% distinct() %>% count() #number of garages where the buses come from
d1 %>% select(lat,lon) %>% distinct() %>% count() # number of distinct bus stops
d1 %>% select(Trans_status) %>% distinct() %>% count() # number of distinct types of transactions
d1 %>% group_by(Trans_status) %>% summarize(n=n()) # count number of each of transaction types
d1 %>% group_by(Hour) %>% summarize(n=n(), sum = sum(Count)) # count number of people getting on at each hour
#d1 %>% group_by(OPERATOR) %>% summarize(n=n()) # 184318 PACE buses
d1 %>% group_by(ROUTE_Number) %>% summarize(n=n()) # frequency of each route used
d1 %>% group_by(DIRECTION) %>% summarize(n=n()) # check distributions of routes with certain directions
d1 %>% group_by(Facility) %>% summarize(n = n())
d1 %>% group_by(START_TIME) %>% summarize(n=n())

d3 <- d1[order(d1$Hour, decreasing=FALSE),]
d3_new <- d1_new[order(d1_new$Hour, decreasing=FALSE),]
#update d3 with subset
d3 = d3[c("lat", "lon", "Hour", "Count", "ROUTE_Number", "START_TIME")]
d3_new = d3_new[c("lat", "lon", "Hour", "Count", "ROUTE_Number", "START_TIME")]
ON_SUM = d3 %>% group_by(Hour) %>% summarize(n=n(), sum = sum(as.numeric(Count)))
ON_SUM_new = d3_new %>% group_by(Hour) %>% summarize(n=n(), sum = sum(as.numeric(Count)))
ON_AVG = d3 %>% group_by(Hour, lat, lon) %>% summarize(n=n(), mean = mean(as.numeric(Count)))
ON_AVG_new = d3_new %>% group_by(Hour, lat, lon) %>% summarize(n=n(), mean = mean(as.numeric(Count)))
#per day of month
ON_SUM2 = d3 %>% group_by(substr(START_TIME,4,5)) %>% summarize(n=n(), sum = sum(as.numeric(Count)))
ON_SUM2_new = d3_new %>% group_by(START_TIME) %>% summarize(n=n(), sum = sum(as.numeric(Count)))
ON_AVG2 = d3 %>% group_by(substr(START_TIME,4,5)) %>% summarize(n=n(), mean = mean(as.numeric(Count)))
ON_AVG2_new = d3_new %>% group_by(START_TIME) %>% summarize(n=n(), mean = mean(as.numeric(Count)))

#two bar plots for average and total ON counts per hour of day with old dataset
par(mfrow = c(2,1))
TOTAL_VENTRAON <- ON_SUM$sum
MEAN_VENTRAON <- ON_AVG$mean
Hourofday <- ON_AVG$Hour
ggplot(ON_AVG,aes(y= MEAN_VENTRAON, x = Hourofday)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per hour of a day") 
ggplot(ON_SUM,aes(y= TOTAL_VENTRAON, x = ON_SUM$Hour)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per hour of a day")

#create two bar plots for average and total ON counts per hour of a day with new dataset
par(mfrow = c(2,1))
TOTAL_VENTRAON_new <- ON_SUM_new$sum
MEAN_VENTRAON_new <- ON_AVG_new$mean
Hourofday_new <- ON_AVG_new$Hour
Hourofday_new2 <- ON_SUM_new$Hour

ggplot(ON_AVG_new,aes(y= MEAN_VENTRAON_new, x = Hourofday_new)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per hour of a day") 
ggplot(ON_SUM_new,aes(y= TOTAL_VENTRAON_new, x = Hourofday_new2)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per hour of a day") 

#two bar plots
par(mfrow = c(2,1))
TOTAL_VENTRAON2 <- ON_SUM2$sum
MEAN_VENTRAON2 <- ON_AVG2$mean
Dayofmonth <- unique(ON_AVG2$`substr(START_TIME, 4, 5)`)
ggplot(data = ON_AVG2,aes(y= MEAN_VENTRAON2, x = Dayofmonth)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per each day of March 2016") 
ggplot(ON_SUM2,aes(y= TOTAL_VENTRAON2, x = Dayofmonth)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per each day of March 2016") 

par(mfrow = c(2,1))
TOTAL_VENTRAON2_new <- ON_SUM2_new$sum
MEAN_VENTRAON2_new <- ON_AVG2_new$mean
Dayofmonth_new <- unique(substr(ON_AVG2_new$START_TIME,4,5))
ggplot(data = ON_AVG2_new,aes(y= MEAN_VENTRAON2_new, x = Dayofmonth_new)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per each day of March 2016") 
ggplot(ON_SUM2,aes(y= TOTAL_VENTRAON2, x = Dayofmonth_new)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per each day of March 2016") 

#use a new variable d4 and d5 so that START_TIME is in the correct format
d4 <- d1[order(d1$START_TIME, decreasing=FALSE),]
d4_new <- d1_new[order(d1_new$START_TIME, decreasing=FALSE),]
d5 <- d4 %>% select(START_TIME, Count) %>% mutate(weekday = wday(START_TIME, label=TRUE))
d5_new <- d4_new %>% select(START_TIME, Count) %>% mutate(weekday = wday(START_TIME, label=TRUE))
#compute total ON count data per days of week
ON_SUM3 <- d5%>% group_by(weekday) %>%summarize(n=n(), sum=sum(as.numeric(Count)))
ON_SUM3_new <- d5_new%>% group_by(weekday) %>%summarize(n=n(), sum=sum(as.numeric(Count)))
#re-order the data frame based on Mon - Sun
ON_SUM3$weekday <- factor(ON_SUM3$weekday, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
ON_SUM3_new$weekday <- factor(ON_SUM3_new$weekday, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
#compute average ON count data per days of week
ON_AVG3 <- d5%>% group_by(weekday) %>%summarize(n=n(), mean=mean(as.numeric(Count)))
ON_AVG3_new <- d5_new%>% group_by(weekday) %>%summarize(n=n(), mean=mean(as.numeric(Count)))
#re-order the data frame based on Mon - Sun
ON_AVG3$weekday <- factor(ON_AVG3$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))
ON_AVG3_new$weekday<- factor(ON_AVG3_new$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))

#two bar plots of total and mean ON counts per days of a week
par(mfrow = c(2,1))
TOTAL_VENTRAON3 <- ON_SUM3$sum
MEAN_VENTRAON3 <- ON_AVG3$mean
Dayofweek <- ON_AVG3$weekday
ggplot(data = ON_AVG3,aes(y= MEAN_VENTRAON3, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per each day of week") 
ggplot(data = ON_SUM3,aes(y= TOTAL_VENTRAON3, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per each day of week") 

#two bar plots of total and mean ON counts per days of a week - different dataset
par(mfrow = c(2,1))
TOTAL_VENTRAON3_new <- ON_SUM3_new$sum
MEAN_VENTRAON3_new <- ON_AVG3_new$mean
Dayofweek_new <- ON_AVG3_new$weekday
ggplot(data = ON_AVG3_new,aes(y= MEAN_VENTRAON3_new, x = Dayofweek_new)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per each day of week") 
ggplot(data = ON_SUM3_new,aes(y= TOTAL_VENTRAON3_new, x = Dayofweek_new)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per each day of week") 

# compute total and average on-boarding passengers per route per day of month
SUM_DOM =  d1 %>% group_by(ROUTE_Number, START_TIME) %>% summarize(n = n(), sum = sum(as.numeric(Count)))
AVG_DOM =  d1 %>% group_by(ROUTE_Number, START_TIME) %>% summarize(n = n(), mean = mean(as.numeric(Count)))

#Bar plots of total ON counts in each route per each day of March 2016
plot_list = list()
for (i in unique(SUM_DOM$ROUTE_Number)) {
  newdata <- subset(SUM_DOM, SUM_DOM$ROUTE_Number == as.character(i))
  Day <- unique(substr(newdata$START_TIME,4,5))
  APCON <- newdata$sum
  p = ggplot(newdata,aes(x = Day, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Total on-boarding passengers per route", i, "per each day of March 2016"))
  plot_list[[i]] = p
  file_name = paste("Total on-boarding passengers per route", i, "per each day of March 2016.jpg")
  jpeg(file_name)
  print(plot_list[[i]])
  dev.off()
}
# create bar plots for average ON counts in route i per each day of the month
plot_list2 = list()
for (i in unique(AVG_DOM$ROUTE_Number)) {
  newdata2 <- subset(AVG_DOM, AVG_DOM$ROUTE_Number == as.character(i))
  Day <- unique(substr(newdata2$START_TIME,4,5))
  APCON <- newdata2$mean
  p = ggplot(newdata2,aes(x = Day, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Average on-boarding passengers per route", i, "per each day of March 2016"))
  plot_list2[[i]] = p
  file_name = paste("Average on-boarding passengers per route", i, "per each day of March 2016.jpg")
  jpeg(file_name)
  print(plot_list2[[i]])
  dev.off()
}

# compute total and average on-boarding passengers per route per hour of day
SUM_HOD =  d1 %>% group_by(ROUTE_Number, Hour) %>% summarize(n = n(), sum = sum(as.numeric(Count)))
AVG_HOD =  d1 %>% group_by(ROUTE_Number, Hour) %>% summarize(n = n(), mean = mean(as.numeric(Count)))

#Bar plots of total ON counts in each route i across different hours of a day
plot_list3 = list()
for (i in unique(SUM_HOD$ROUTE_Number)) {
  newdata <- subset(SUM_HOD, SUM_HOD$ROUTE_Number == as.character(i))
  Hour <- unique(newdata$Hour)
  APCON <- newdata$sum
  p = ggplot(newdata,aes(x = Hour, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Total number of on-boarding passengers per route", i, "by hours"))
  plot_list3[[i]] = p
  file_name = paste("Total number of on-boarding passengers per route", i, "by hours.jpg")
  jpeg(file_name)
  print(plot_list3[[i]])
  dev.off()
}
#create every bar plots for average ON count in each route across hours of a day
plot_list4 = list()
for (i in unique(AVG_HOD$ROUTE_Number)) {
  newdata2 <- subset(AVG_HOD, AVG_HOD$ROUTE_Number == as.character(i))
  Hour <- unique(newdata2$Hour)
  APCON <- newdata2$mean
  p = ggplot(newdata2,aes(x = Hour, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Average number of on-boarding passengers per route", i, "by hours"))
  plot_list4[[i]] = p
  file_name = paste("Average on-boarding passengers per route", i, "by hours.jpg")
  jpeg(file_name)
  print(plot_list4[[i]])
  dev.off()
}

d6 <- d4 %>% select(ROUTE_Number, START_TIME, Count, lat, lon) %>% mutate(weekday = wday(START_TIME, label=TRUE))
#aggregate total and ON count data per each route per each day of week
ON_SUM4 <- d6%>% group_by(ROUTE_Number, weekday) %>%summarize(n=n(), sum=sum(as.numeric(Count)))
ON_SUM4$weekday <- factor(ON_SUM4$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))
ON_AVG4 <- d6%>% group_by(ROUTE_Number,weekday) %>%summarize(n=n(), mean=mean(as.numeric(Count)))
ON_AVG4$weekday <- factor(ON_AVG4$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))

#Create every bar plots of total and average on-boarding passsengers per route per days of week
plot_list5 = list()
for (i in unique(ON_SUM4$ROUTE_Number)) {
  newdata <- subset(ON_SUM4, ON_SUM4$ROUTE_Number == i)
  Dayofweek <- unique(newdata$weekday)
  APCON <- newdata$sum
  p = ggplot(newdata,aes(x = Dayofweek, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Total number of on-boarding passengers per route", i, "by day of week"))
  plot_list5[[i]] = p
  file_name = paste("Total number of on-boarding passengers per route", i, "by day of week.jpg")
  jpeg(file_name)
  print(plot_list5[[i]])
  dev.off()
}

plot_list6 = list()
for (i in unique(ON_AVG4$ROUTE_Number)) {
  newdata2 <- subset(ON_AVG4, ON_AVG4$ROUTE_Number == as.character(i))
  Dayofweek <- unique(newdata2$weekday)
  APCON <- newdata2$mean
  p = ggplot(newdata2,aes(x = Dayofweek, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Average number of on-boarding passengers per route", i, "by day of week"))
  plot_list6[[i]] = p
  file_name = paste("Average on-boarding passengers per route", i, "by day of week.jpg")
  jpeg(file_name)
  print(plot_list6[[i]])
  dev.off()
}

install.packages("ggmap", type = "source")
install.packages("gridExtra")
library(gridExtra)
library(scales)
library(ggmap)

require('ggmap')
d5 <- d1 %>% select(col_title) %>% mutate(weekday = wday(START_TIME, label=TRUE))
d5$weekday <- factor(d5$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))
map.in3 <- get_map(location = c(min(d5$lon)-0.1,
                                min(d5$lat)-0.1,
                                max(d5$lon)+0.1,
                                max(d5$lat)+0.1),
                  source = "google")

theme_set(theme_bw(base_size = 8))
colormap <- c("Violet","Blue","Green","Yellow","Red","White")

#generate heat map for average number of on-boarding passengers per days of week
pred.stat.map.final <- ggmap(map.in3) %+% d5 + 
  aes(x = lon,
      y = lat,
      z = as.numeric(Count)) +
  stat_summary_2d(fun = mean, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Mean",
                       colours =  YlOrBr,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of Average number of on-boarding passengers per day of week") + facet_wrap(~ weekday) + 
  coord_map()
print(pred.stat.map.final)

#generate heat map for total number of on-boarding passengers per days of week
pred.stat.map.final2 <- ggmap(map.in3) %+% d5 + 
  aes(x = lon,
      y = lat,
      z = as.numeric(Count)) +
  stat_summary_2d(fun = sum, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Sum",
                       colours =  YlOrBr,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of Total number of on-boarding passengers per day of week") + facet_wrap(~ weekday) + 
  coord_map()
print(pred.stat.map.final2)

#generate heat map for average number of on-boarding passengers per hours of day
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
d9 = d1 %>% filter(lat > 41, lon < -85)
#d9 <- d9 %>% group_by(Hour) %>% summarize(n=n(), sum = sum(as.numeric(Count)))
map.in4 <- get_map(location = c(min(d9$lon)-0.05,
                                min(d9$lat)-0.05,
                                max(d9$lon)+0.1,
                                max(d9$lat)+0.1),
                   source = "google")

pred.stat.map.final5 <- ggmap(map.in4) %+% d9 + 
  aes(x = lon,
      y = lat,
      z = as.numeric(Count)) +
  stat_summary_2d(fun = sum, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Total",
                       colors = terrain.colors(10),
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of total number of on-boarding passengers per day of month") + facet_wrap(~ START_TIME) + 
  coord_map()
print(pred.stat.map.final5)

#generate heat map for average number of on-boarding passengers per days of month
pred.stat.map.final6 <- ggmap(map.in4) %+% d9 + 
  aes(x = lon,
      y = lat,
      z = as.numeric(Count)) +
  stat_summary_2d(fun = mean, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Sum",
                       colours =  terrain.colors(10),
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of Average number of on-boarding passengers per days of month") + facet_wrap(~ START_TIME) + 
  coord_map()
print(pred.stat.map.final6)

#generate heat map for total number of on-boarding passengers per hours of day
pred.stat.map.final7 <- ggmap(map.in4) %+% d9 + 
  aes(x = lon,
      y = lat,
      z = as.numeric(Count)) +
  stat_summary_2d(fun = sum, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Total",
                       colors = terrain.colors(10),
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of total number of on-boarding passengers per hours of day") + facet_wrap(~ Hour) + 
  coord_map()
print(pred.stat.map.final7)

#generate heat map for average number of on-boarding passengers per hours of day
pred.stat.map.final8 <- ggmap(map.in4) %+% d9 + 
  aes(x = lon,
      y = lat,
      z = as.numeric(Count)) +
  stat_summary_2d(fun = mean, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Sum",
                       colours =  terrain.colors(10),
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of Average number of on-boarding passengers per hours of day") + facet_wrap(~ Hour) + 
  coord_map()
print(pred.stat.map.final8)

#generate heat map for total number of on-boarding passengers per hours of day
pred.stat.map.final7 <- ggmap(map.in4) %+% d9 + 
  aes(x = lon,
      y = lat,
      z = as.numeric(Count)) +
  stat_summary_2d(fun = sum, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Total",
                       colors = terrain.colors(10),
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of total number of on-boarding passengers per hours of day") + facet_wrap(~ Hour) + 
  coord_map()
print(pred.stat.map.final7)

#generate heat map for total number of each transaction type per hours of day
d10 <- d9%>% group_by(Trans_status, lat, lon, Hour) %>% summarize(n=n())
map.in5 <- get_map(location = c(min(d10$lon)-0.05,
                                min(d10$lat)-0.05,
                                max(d10$lon)+0.03,
                                max(d10$lat)+0.05),
                   source = "google")
pred.stat.map.final8 <- ggmap(map.in5) %+% d10 + 
  aes(x = lon,
      y = lat,
      z = n) +
  stat_summary_2d(fun = sum, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Total Transfer Status",
                       colours =  terrain.colors(10),
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of total of each of transfer status of on-boarding passengers") + facet_wrap(~ Trans_status) + 
  coord_map()
print(pred.stat.map.final8)

pred.stat.map.final9 <- ggmap(map.in5) %+% d10 + 
  aes(x = lon,
      y = lat,
      z = n) +
  stat_summary_2d(fun = mean, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Average Transfer Status",
                       colours =  terrain.colors(10),
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of average of each of transfer status of on-boarding passengers") + facet_wrap(~ Trans_status) + 
  coord_map()
print(pred.stat.map.final9)

d11 <- d1[order(d1$Hour, decreasing=FALSE),]
#update d3 with subset
d11 = d11[c("lat", "lon", "Hour", "Count", "Trans_status", "START_TIME", "TRIP_TYPE")]
d11[c("TRIP_TYPE")][is.na(d11[c("TRIP_TYPE")])] <- 2
d11$START_TIME <- substr(d1$START_TIME,9,10)
d11 <- d11[order(d11$START_TIME, decreasing=FALSE),]
d12 = d11 %>% group_by(Trans_status, Hour) %>% summarize(mean = mean(as.numeric(Count)))
d13 = d11 %>% group_by(Trans_status, START_TIME) %>% summarize (mean = mean(as.numeric(Count)))
d14 = d11 %>% group_by(TRIP_TYPE, Hour) %>% summarize(mean = mean(as.numeric(Count)))
d15 = d11 %>% group_by(TRIP_TYPE, START_TIME)%>% summarize(mean = mean(as.numeric(Count)))

#barplot for total transfer status per hours of day
tbl <- with(d11, table(Trans_status, Hour))
tbl3 <- with(d11, table(Trans_status, START_TIME))
tbl2 <- with(d11, table(TRIP_TYPE, Hour))
tbl4 <- with(d11, table(TRIP_TYPE, START_TIME))


#bartplot for total transfer per hours of day
ggplot(as.data.frame(tbl), aes(factor(Hour), Freq, fill = Trans_status)) +     
  geom_col(position = 'dodge') + labs(x = "Hour",
                                      y = "Total Transfers", title = "Bar plot for transfer statuses per hour")

#bartplot for total transfer per days of month
ggplot(as.data.frame(tbl3), aes(factor(START_TIME), Freq, fill = Trans_status)) +     
  geom_col(position = 'dodge') + labs(x = "Day of month",
                                      y = "Total Transfers", title = "Bar plot for transfer statuses per days of month")

#bartplot for average transfer per days of month
ggplot(data = d13, aes(factor(START_TIME), y = mean, fill = Trans_status)) +     
  geom_col(position = 'dodge') + labs(x = "Day of month",
                                      y = "Average Transfers", title = "Bar plot for average transfer statuses per days of month")

#barplot for average transfer status per hours of day
ggplot(data = d12, aes(x = factor(Hour), y = mean, fill = Trans_status)) + geom_bar(stat = "identity") + labs(x = "Hour", y = "Average Number of Transfers", title = "Bar plot for average number of transfer statuses per hour")

#barplot for total trip types per hours of day
ggplot(as.data.frame(tbl2), aes(factor(Hour), Freq, fill = TRIP_TYPE)) +     
  geom_col(position = 'dodge') + labs(x = "Hour",
                                      y = "Total trips",title = "Barplot for total trip types per hour") + scale_color_manual(labels = c("0", "1", NA))

#barplot for average trip types per days of month
ggplot(data = d15, aes(x = factor(START_TIME), y = mean, fill = TRIP_TYPE)) + geom_bar(stat = "identity") + labs(x = "Hour", y = "Average Number of Trips", title = "Barplot for average number of trip types per days of Month")

#barplot for total trip types per days of month
ggplot(as.data.frame(tbl4), aes(x = factor(START_TIME), Freq, fill = TRIP_TYPE)) + geom_bar(stat = "identity") + labs(x = "Hour", y = "Total Number of Trips", title = "Barplot for total number of trip types per days of month")



