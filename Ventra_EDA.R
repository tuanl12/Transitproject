install.packages("ggmap")
install.packages("ggplot2")
install.packages("ggproto")
install.packages("chron")
install.packages("scales")
install.packages(c("dbplyr", "RSQLite"))

library(ggmap)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(dbplyr)
library(reshape)
library(reshape2)
setwd("~/Dropbox/proj/fta/")

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
total_list2 <- total_list[!(total_list$X33 == "No Payment"),]

d1 = dplyr::rename(total_list, lat="X3", lon="X4", DW_TRANSACTION_ID=X1, DEVICE_ID=X2, BENEFIT_VALUE=X5, TRANSACTION_DTM=X64, PACE_RUN_ID=X20, DIRECTION=X58, START_TIME=X61, TRIP_TYPE=X71, Hour=X24, Count=X19,ROUTE_Number=X51, COST=X26, Facility=X12, Trans_status=X33, OPERATOR=X47, STOP=X50, PREV_LATITUDE=X76, PREV_LONGITUDE=X77, CARD_TYPE=X46)
d1$CASH_YESNO <- ifelse(d1$Trans_status == "No Payment", "YES", "NO")

d1_new = dplyr::rename(total_list2, lat=X3, lon=X4, DW_TRANSACTION_ID=X1, DEVICE_ID=X2, BENEFIT_VALUE=X5, TRANSACTION_DTM=X64, PACE_RUN_ID=X20, DIRECTION=X58, START_TIME=X61, TRIP_TYPE=X71, Hour=X24, Count=X19,ROUTE_Number=X51, COST=X26, Facility=X12, Trans_status=X33, OPERATOR=X47, STOP=X49, PREV_LATITUDE=X76, PREV_LONGITUDE=X77, CARD_TYPE=X46)
col_title <- c("lat", "lon", "START_TIME", "DIRECTION", 
          "Hour", "TRIP_TYPE", "STOP", "COST", "Count", "ROUTE_Number", "Trans_status")
d1 =  d1[col_title]
d1_new = d1_new[col_title]
d1$Hour <- substr(d1$Hour,1,2) 
d1_new$Hour <- substr(d1_new$Hour,1,2)
d1$START_TIME <- substr(d1$START_TIME,6,10)
d1_new$START_TIME <- substr(d1_new$START_TIME,6, 10)

d1 = mutate(d1, lat = as.numeric(lat))
d1_new = mutate(d1_new, lat = as.numeric(lat))
d1 = mutate(d1, lon = as.numeric(lon))
d1_new = mutate(d1_new, lon = as.numeric(lon))
d1$lat <- as.numeric(d1$lat)/1e6
d1_new$lat <- as.numeric(d1_new$lat)/1e6
d1$lon <- as.numeric(d1$lon)/1e6
d1_new$lon <- as.numeric(d1_new$lon)/1e6

d1 <- d1[!(is.na(d1$lat)) & !(is.na(d1$lon)),]
d1 = d1 %>% filter(lat > 20, lon < - 86)
d1_new = d1_new %>% filter(lat>41, lon<-85)

stops <- read_delim("/Users/tuanle/stop_zone.csv", delim = ",", quote = '', col_names = FALSE, col_types = cols(.default = "c"))
stops <- stops[, c("X3", "X5", "X6", "X8")]
d1["ZONE"] <- NA
#eliminate all duplicated transaction for storing repriced trip data
d1_valid <- d1[d1$Trans_status != "Transaction created to store repriced trip data",]
#assign zones to corresponding bus stops
#for (i in seq_len(length(d1_valid$lat))){
    for(k in (2:length(stops$X5))){
      if(d1_valid$lat[3] == as.numeric(stops$X5[k]) && d1_valid$lon[3] == as.numeric(stops$X6[k])){
        d1_valid$ZONE[3] <- stops$X8[k]
      } 
    }
#}
require(ggmap)
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

par(mfrow = c(2,1))
TOTAL_VENTRAON <- ON_SUM$sum
MEAN_VENTRAON <- ON_AVG$mean
Hourofday <- ON_AVG$Hour
ggplot(ON_AVG,aes(y= MEAN_VENTRAON, x = Hourofday)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per hour of a day") 
ggplot(ON_SUM,aes(y= TOTAL_VENTRAON, x = ON_SUM$Hour)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per hour of a day")

#new dataset d2_new
par(mfrow = c(2,1))
TOTAL_VENTRAON_new <- ON_SUM_new$sum
MEAN_VENTRAON_new <- ON_AVG_new$mean
Hourofday_new <- ON_AVG_new$Hour
Hourofday_new2 <- ON_SUM_new$Hour
ggplot(ON_AVG_new,aes(y= MEAN_VENTRAON_new, x = Hourofday_new)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per hour of a day") 

ggplot(ON_SUM_new,aes(y= TOTAL_VENTRAON_new, x = Hourofday_new2)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per hour of a day") 

#histogram by days of month (March 2016)
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
ON_SUM3 <- d5%>% group_by(weekday) %>%summarize(n=n(), sum=sum(as.numeric(Count)))
ON_SUM3_new <- d5_new%>% group_by(weekday) %>%summarize(n=n(), sum=sum(as.numeric(Count)))
ON_SUM3$weekday <- factor(ON_SUM3$weekday, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
ON_SUM3_new$weekday <- factor(ON_SUM3_new$weekday, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
ON_AVG3 <- d5%>% group_by(weekday) %>%summarize(n=n(), mean=mean(as.numeric(Count)))
ON_AVG3_new <- d5_new%>% group_by(weekday) %>%summarize(n=n(), mean=mean(as.numeric(Count)))
ON_AVG3$weekday <- factor(ON_AVG3$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))
ON_AVG3_new$weekday<- factor(ON_AVG3_new$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))

#histograms by day of week
par(mfrow = c(2,1))
TOTAL_VENTRAON3 <- ON_SUM3$sum
MEAN_VENTRAON3 <- ON_AVG3$mean
Dayofweek <- ON_AVG3$weekday
ggplot(data = ON_AVG3,aes(y= MEAN_VENTRAON3, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per each day of week") 
ggplot(data = ON_SUM3,aes(y= TOTAL_VENTRAON3, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per each day of week") 

par(mfrow = c(2,1))
TOTAL_VENTRAON3_new <- ON_SUM3_new$sum
MEAN_VENTRAON3_new <- ON_AVG3_new$mean
Dayofweek_new <- ON_AVG3_new$weekday
ggplot(data = ON_AVG3_new,aes(y= MEAN_VENTRAON3_new, x = Dayofweek_new)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per each day of week") 
ggplot(data = ON_SUM3_new,aes(y= TOTAL_VENTRAON3_new, x = Dayofweek_new)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per each day of week") 

# compute total and average on-boarding passengers per route per day of month
SUM_DOM =  d1 %>% group_by(ROUTE_Number, START_TIME) %>% summarize(n = n(), sum = sum(as.numeric(Count)))
AVG_DOM =  d1 %>% group_by(ROUTE_Number, START_TIME) %>% summarize(n = n(), mean = mean(as.numeric(Count)))

#Histograms of total and average on-boarding passsengers per route per each day of March 2016
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

#Histograms of total and average on-boarding passsengers per route per hour of day
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

ON_SUM4 <- d6%>% group_by(ROUTE_Number, weekday) %>%summarize(n=n(), sum=sum(as.numeric(Count)))
ON_SUM4$weekday <- factor(ON_SUM4$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))
ON_AVG4 <- d6%>% group_by(ROUTE_Number,weekday) %>%summarize(n=n(), mean=mean(as.numeric(Count)))
ON_AVG4$weekday <- factor(ON_AVG4$weekday, levels = c("Mon","Tue", "Wed","Thu", "Fri", "Sat","Sun"))

#Histograms of total and average on-boarding passsengers per route per days of week
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

# #bar plot for total trip types per hours of day
# library(reshape2)
# d12 <- d11 %>% group_by(TRIP_TYPE, Hour) %>% summarize(n=n())
# d12 <- melt(data = d12)
# ggplot(data = d12, aes(x = factor(Hour), y = value, fill = TRIP_TYPE)) +     
#   geom_col(position = 'dodge') + labs(x = "Hour",
#                                       y = "Total trips",title = "Bar plot for trip types per hour") 

d_stop = read.csv("/Users/tuanle/stops.txt", sep = ',', stringsAsFactors = F)
d_stop <- t(na.omit(t(d_stop)))
colnames(d_stop)[3] <- "lat"
colnames(d_stop)[4] <- "lon"
d_stop <- as.data.frame(d_stop)
library(data.table)


ventra=data.frame(
  DW_TRANSACTION_ID=character(),
  DEVICE_ID=character(),
  LATITUDE=character(),
  LONGITUDE=character(),
  BENEFIT_VALUE=double(),
  BUS_ID=double(),
  CARD_KEY=double(),
  CARD_SEQUENCE_NBR=double(),
  CELLULAR_CONNECTION_STATUS=double(),
  CELLULAR_SIGNAL_STRENGTH=double(),
  FACILITY_ID=double(),
  FACILITY_NAME=character(),
  FARE_TABLE_ID=double(),
  PATRON_COUNT=double(),
  POSTING_DAY_KEY=double(),
  RENEWED_IN_ADVANCE=double(),
  RIDES_REMAINING=double(),
  PAYGO_RIDE_COUNT=double(),
  RIDE_COUNT=double(),
  PACE_RUN_ID=character(),
  SERIAL_NBR=character(),
  SOURCE=character(),
  SV_ADJUSTED_TXN=double(),
  TIME_INCREMENT_KEY=character(),
  TIME_PERIOD_KEY=double(),
  TOTAL_COST=double(),
  TRANSACTION_ID=double(),
  TRANSFER_CODE=double(),
  TRANSFER_COUNT=double(),
  TRANSIT_ACCOUNT_ID=double(),
  TRANSIT_DAY_KEY=double(),
  TXN_DESC=character(),
  TXN_STATUS_DESC=character(),
  TXN_STATUS_NAME=character(),
  VALUE_CHANGED=double(),
  VALUE_REMAINING=double(),
  FARE_PROD_ID=double(),
  FARE_PROD_NAME=character(),
  RIDER_CLASS_ID=double(),
  RIDER_CLASS_NAME=character(),
  TICKET_TYPE_ID=double(),
  TICKET_TYPE_NAME=character(),
  FARE_PROD_CATEGORY_NAME=character(),
  LAST_USE_OPERATOR_KEY=double(),
  MEDIA_TYPE_ID=double(),
  MEDIA_TYPE_NAME=character(),
  OPERATOR_ID=double(),
  OPERATOR_NAME=character(),
  PREVIOUS_STOP_POINT_KEY=double(),
  ROUTE_DESC=character(),
  ROUTE_number=double(),
  BANKCARD_PAYMENT_VALUE=double(),
  FARE_DUE=double(),
  MULTI_RIDE_ID=double(),
  PASS_COST=double(),
  PASS_USE_COUNT=double(),
  SETTLEMENT_DAY_KEY=double(),
  TRIP_INFO_DIRECTION=character(),
  TAP_ID=double(),
  TRIP_PRICE_COUNT=double(),
  TRANSACTION_DTM=character(),
  SOURCE_INSERTED_DTM=character(),
  STAGING_INSERTED_DTM=character(),
  EDW_INSERTED_DTM=character(),
  SOURCE_TRIP_PRICED_DTM=character(),
  PRICED_DTM=character(),
  SETTLEMENT_DAY_KEY_SET_DTM=character(),
  JOURNEY_START_DTM=character(),
  PATRON_TRIP_ID=double(),
  PATRON_JOURNEY_ID=double(),
  TRANSFER_SEQUENCE_NBR=double(),
  TRANSFER_FLAG=double(),
  FIRST_PREV_OPERATOR_ID=double(),
  FIRST_PREV_ROUTE_OR_STATION=character(),
  FIRST_PREV_TRANSACTION_DTM=character(),
  FIRST_PREV_LATITUDE=character(),
  FIRST_PREV_LONGITUDE=character(),
  FIRST_PREV_TRANSFER_SEQ_NBR=double(),
  FIRST_PREV_TRANSFER_FLAG=double(),
  FIRST_PREV_ROUTE_STATION_DESC=character(),
  SECOND_PREV_OPERATOR_ID=double(),
  SECOND_PREV_ROUTE_OR_STATION=character(),
  SECOND_PREV_TRANSACTION_DTM=character(),
  SECOND_PREV_LATITUDE=character(),
  SECOND_PREV_LONGITUDE=character(),
  SECOND_PREV_TRANSFER_SEQ_NBR=double(),
  SECOND_PREV_TRANSFER_FLAG=double(),
  SECOND_PREV_ROUTE_STATION_DESC=character(),
  stringsAsFactors=FALSE
)
fname=unzip("/Users/tuanle/vp080_oct_2015_abp_extract_0001.zip")
d = read_delim(fname[1], col_names = F,quote = '"',delim = "|")

fl=list.files(".", pattern=".zip")
for (fn in fl) {
  unzip_files=unzip(fn)
  for (item in unzip_files) {
    print(item)
    d = read_delim(item, col_names = F,quote = '"',delim = "|", 
                  col_types = cols(.default = "c"))
    ventra = rbind(ventra,d[,1:88])
    file.remove(item)
  }
}

library(dplyr)
library(RSQLite)
library(sqldf)
devtools::install_github("ggrothendieck/sqldf", force = TRUE)
#con <- DBI::dbConnect(SQLite(), dbname = 'ventra.sqlite')
#Create an empty SQLite-database
db <- dbConnect(SQLite(), dbname= "ventra.sqlite")
#sqldf("SELECT * FROM data_db", dbname = "ventra.sqlite")
#Paste a table named data_db into the "ventra.sqlite" database
dbWriteTable(conn = db, name = "data_db", value = data_db, row.names = FALSE, header = TRUE)
#Test by querying a random column => Work!
sql1 <- paste("SELECT data_db.TXN_STATUS_DESC FROM data_db", sep="")
results <- dbGetQuery(db, sql1)
ventra <- paste("SELECT * FROM data_db", sep ="")
ventra <- as.data.frame(dbGetQuery(db, ventra))
#Disconnection from databaset
dbDisconnect(db)
# ventra <- as.data.frame(ventra)
# copy_to(dest = con, df = ventra, name = "ventra", overwrite = T)
# data_db <-  tbl("select (*) from ventra")
#data_db <- as.data.frame(ventra)
#d1_db = dplyr::rename(data_db, lat=X3, lon=X4, DW_TRANSACTION_ID=X1, DEVICE_ID=X2, BENEFIT_VALUE=X5, TRANSACTION_DTM=X64, PACE_RUN_ID=X20, DIRECTION=X58, START_TIME=X61, TRIP_TYPE=X71, Hour=X24, Count=X19,ROUTE_Number=X51, COST=X26, Facility=X12, Trans_status=X33, OPERATOR=X47, STOP=X49, PREV_LATITUDE=X76, PREV_LONGITUDE=X77, CARD_TYPE=X46)
d1_db = ventra
d1_db$CASH_YESNO <- ifelse(d1_db$TXN_STATUS_DESC == "No Payment", "YES", "NO")
col_title <- c("LATITUDE", "LONGITUDE", "TRANSACTION_DTM", "TRIP_INFO_DIRECTION", 
               "TRANSFER_SEQUENCE_NBR", "PREVIOUS_STOP_POINT_KEY", "TOTAL_COST", "RIDE_COUNT", "ROUTE_number", "TXN_STATUS_DESC", "CASH_YESNO")
d1_db =  d1_db[col_title]
d1_db$Hour <- substr(d1_db$TRANSACTION_DTM,12,13) 
d1_db$TRANSACTION_DTM <- substr(d1_db$TRANSACTION_DTM,6,10)
d1_db = dplyr::rename(d1_db, lat = LATITUDE, lon = LONGITUDE, date = TRANSACTION_DTM, direction = TRIP_INFO_DIRECTION, 
                      transfer_status = TRANSFER_SEQUENCE_NBR, prev_stop = PREVIOUS_STOP_POINT_KEY, cost = TOTAL_COST, count = RIDE_COUNT, ROUTE_Number=ROUTE_number,  Trans_status = TXN_STATUS_DESC)

d1_db = mutate(d1_db, lat = as.numeric(lat))
d1_db = mutate(d1_db, lon = as.numeric(lon))
d1_db$lat <- as.numeric(d1_db$lat)/1e6
d1_db$lon <- as.numeric(d1_db$lon)/1e6

d1_db <- d1_db[!(is.na(d1_db$lat)) & !(is.na(d1_db$lon)),] #3.54% of our combined dataset has NA entry in lat or lon

eps = 0.05 #distance between recorded lon and the nearest bus stop, taken from Northwestern's presentation
for (i in 1:length(d1_db$lat)) {
    for(j in 1:length(d_stop$lat)) {
           if(#condition for recorded locations to be within r = 0.05 miles of the nearest bus stops
             (sqrt((as.numeric(d1_db$lon[i]) - as.numeric(d_stop$lon[j]))^2 
                         + (as.numeric(d1_db$lat[i]) - as.numeric(d_stop$lat[j]))^2) < eps) 
             & (abs(as.numeric(d1_db$lon[i]) - as.numeric(d_stop$lon[j])) < eps)
             & as.numeric(d1_db$lon[i]) > as.numeric(d_stop$lon[j])
             )
             {d1_db$stop_name[i] <- as.character(d_stop$stop_name[j])}
    }
    d1_db$stop_name[i] <- "NA"
  }

d2_db = d1_db
d2_db = d2_db %>% filter(lat > 41, lat < 50, lon > - 90, lon < -70)

require(ggmap)
stops = d2_db %>% select(lat,lon) %>% distinct()
sbbox <- make_bbox(lon = as.numeric(stops$lon), lat = as.numeric(stops$lat), f = .05)
sq_map <- get_map(location = sbbox, source = "google")
ggmap(sq_map) + geom_point(data = stops, mapping = aes(x = lon, y = lat, color = "red"))
region <- get_map(location = c(min(d1$lon),
                               min(d1$lat),
                               max(d1$lon),
                               max(d1$lat)),
                  source = "google")

d3 <- d1_db[order(d1_db$Hour, decreasing=FALSE),]
#update d3 with subset
d3 = d3[c("lat", "lon", "Hour", "Count", "ROUTE_Number", "START_TIME")]
ON_SUM = d3 %>% group_by(Hour) %>% summarize(n=n(), sum = sum(as.numeric(Count)))
ON_AVG = d3 %>% group_by(Hour, lat, lon) %>% summarize(n=n(), mean = mean(as.numeric(Count)))
#per day of month
ON_SUM2 = d3 %>% group_by(substr(START_TIME,4,5)) %>% summarize(n=n(), sum = sum(as.numeric(Count)))
ON_AVG2 = d3 %>% group_by(substr(START_TIME,4,5)) %>% summarize(n=n(), mean = mean(as.numeric(Count)))

par(mfrow = c(2,1))
TOTAL_VENTRAON <- ON_SUM$sum
MEAN_VENTRAON <- ON_AVG$mean
Hourofday <- ON_AVG$Hour
ggplot(ON_AVG,aes(y= MEAN_VENTRAON, x = Hourofday)) + geom_bar(stat="identity") + ggtitle("Average on-boarding passengers per hour of a day") 
ggplot(ON_SUM,aes(y= TOTAL_VENTRAON, x = ON_SUM$Hour)) + geom_bar(stat="identity") + ggtitle("Total on-boarding passengers per hour of a day")

dbDisconnect()
# library(sqldf)
# f <- file("20160301_20160331_vp080_abp_tran_extract_0001.dat")
# system.time(bigdf <- sqldf("select * from f", dbname = tempfile(), file.format = list(header = F, row.names = F)))
