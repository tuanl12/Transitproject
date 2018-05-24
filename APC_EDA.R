#install essential packages
install.packages("tidyverse")
install.packages("hash")
install.packages("ggmap")
install.packages("ddply")
install.packages("ggproto")
install.packages("chron")
install.packages("scales")

#load all essential library just in case!
library(hash)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(lubridate)
library(xts)
library(chron)
library(dplyr)

#create apc data frame with column headers and data types under each column.
apc =data.frame(
  Date=character(),
  DOW=character(),
  Block=character(),
  Run=double(),
  Route=double(),
  Direction=character(),
  Trip_ID=double(),
  GeoNodeId=double(),
  Stop_Name=character(),
  Stop_Sequence=double(),
  Lat=double(),
  Long=double(),
  ON=double(),
  OFF=double(),
  Load=double(),
  BusID=double(),
  stringsAsFactors=FALSE
)

#read all the data files whose filename end with "2015.txt" and combine them into the apc data frame
fl=list.files(".", pattern="2015.txt")
for (fn in fl) {
  print(fn)
  d = read_delim(fn, col_names = F, delim = ',',
                   col_types = cols(.default = "c"))
  apc = rbind(apc,d[,1:16])
}

#rename all the columns of apc for easy interpretation
apc = dplyr::rename(apc, Date = X1 , DOW = X2, Block = X3, Run = X4, Route = X5, Direction = X6, Trip_ID = X7, 
                       order_numb = X8,  Stop_Name = X9, Stop_Sequence = X10, Lat = X11, Long = X12,  ON = X13, OFF = X14, Load = X15, BusID = X16)

#store apc data into new variable called "d" for data manipulation
d = apc
#create new column named "Date" with only Day part (e.g.01-April becomes 01)
d$Date <- substr(d$Date,1,6)
#create new column named "month" with only month part (e.g. 01-Apr becomes Apr)
d$month <- substr(d$Date,4,6)

#data cleaning ("NA" entries becomes 0 for counts data, eliminate all the rows with NA entry)
d[is.na(d$ON)] <- 0
d[is.na(d$OFF)] <- 0
d <- d[complete.cases(d),]
#convert Lat-Lon from char to numeric type
d$Lat <- as.numeric(d$Lat)
d$Long <- as.numeric(d$Long)
#create a new column called X8 to store "zone" information for each stop
d["X8"] <- NA

#read the file that matches the zone into each stop. 
stops <- read_delim("/Users/tuanle/stop_zone.csv", delim = ",", quote = '', col_names = FALSE, col_types = cols(.default = "c"))
#choose only subsets of data frame stops with columns "X2", "X3", "X5", "X6", "X13" and "X14"
stops <- stops[, c("X2", "X3", "X5", "X6", "X13", "X14")]
#make the data type in columns lat-long from char to numeric
stops$X5[2:length(stops$X5)] <- as.numeric(stops$X5[2:length(stops$X5)])
stops$X6[2:length(stops$X6)] <- as.numeric(stops$X6[2:length(stops$X6)])
#eliminate the first row indicating data type under column header
stops = stops[-1,]
#rename column headers for easy interpretation and data manipulation
stops = dplyr::rename(stops, Lat="X5", Long="X6")
stops = dplyr::rename(stops, Stop_Name = "X3")

#left_join on geonode_ID - the unique ID of each bus stop present in the two files: stop_zone.csv and apc data frame
d_zone <- left_join(x = d, y = stops%>% select("X13", "X14"), by = c("Stop_Sequence" ="X13"))

#column "X14" denote the zone. Aggregate ON/OFF counts data  based on month, date and zone.
d_zone_ON = d_zone%>% group_by(month, Date, X14) %>% summarize(n=n(), ON = sum(as.numeric(ON)))
d_zone_OFF = d_zone%>% group_by(month, Date, X14) %>% summarize(n=n(), ON = sum(as.numeric(OFF)))
d_zone_ONmonth = d_zone_ON%>% group_by(month, X14) %>% summarize(n=n(), ON = sum(as.numeric(ON)))

#eliminate all of the rows with "NA" entries, except the rows corresponding to column "sum". 
#For that column, assign "0" for "NA"
d_zone_ON[is.na(d_zone_ON$n)] <- 0
d_zone_ON <- d_zone_ON[complete.cases(d_zone_ON),]
d_zone_ONmonth[is.na(d_zone_ONmonth$n)] <- 0
d_zone_ONmonth <- d_zone_ONmonth[complete.cases(d_zone_ONmonth),]

#create a vector to store routing matrix in each day (365 days in total)
rte_m <- vector("list", length(unique(d_zone_ON$Date)))

#construct routing matrix A per each day. Note that for our dataset, there is no route having a loop, because length(unique(d_zone_ON$X14(d_zone_ON$Date==j)))-length(d_zone_ON$Date==j) = 0 for all j in Date column.
inx_big = 0
#running for loop through every unique days in the "d_zone_ON" data frame (there are 365 days)
for(j in unique(d_zone_ON$Date)){
  #define the number of rows of routing matrix A, which is equal to the number of elements having a same date "j"
  inx_row <- sum(d_zone_ON$Date== j)
  #define the index of the list rte_m where each element stores a routing matrix
  inx_big <- inx_big+1
  #initiate routing matrix in each day with all entries = 0. Number of columns = number of feasible paths given we have "inx_row" number of elements
  rte_m[[inx_big]] <- data.frame(matrix(0, nrow = inx_row, ncol = inx_row*(inx_row+1)/2))
  #obtain the row index of the first and last element of the "Date" column where it equals to a fixed value "j" 
  lb <- min(which(d_zone_ON$Date == j)) 
  ub <- max(which(d_zone_ON$Date == j)) 
  #running the for loop to construct the column indices of routing matrix A, 
  #which is created as all the pairs (lb,1),(lb,2),...,(lb, ub), (lb+1, 2),(lb+1, 3),....(lb+1, ub),...(ub-1,ub). 
  #The last row's entries are all equal to 0, since that is the last entry corresponds to date "j"
  for(i in lb:ub){
    if(i==lb){
       rte_m[[inx_big]][1,1:(ub-lb)] = 1; #first row's entries that are equal to 1 (the remaining are 0 already)
    }
    else if(i<ub && i> lb){
      #number of entries equal to 1 for all feasible routes that all start with each stop on row "i-lb+1" (e.g: all feasible routes from stop "1000" in date "01-APR").
      #The formula for lower and upper entries are derived entirely based on mathematics + observations from the dataset.
      rte_m[[inx_big]][i-lb+1, ((i-lb)*(ub-lb)-(i-lb+1)*(i-lb-2)/2):((i-lb+1)*(ub-lb)-(i-lb+1)*(i-lb)/2)] = 1; 
    } else {rte_m[[inx_big]][ub-lb+1, ] = 0} #last row's entries are all equal to 0
  }
  #keep track of the iterations. Warning: size of routing matrix is very big, with the total of (hundreds * hundreds of thousands) entries. 
  #Rsession crashes sometime!
  print(j)
  print(nrow(rte_m[[inx_big]])) 
  print(ncol(rte_m[[inx_big]]))
}

#kalman_filter and covariance matrix *list storage* for 365 days in 2015 
kf_vect <- vector("list", 365)
cov_m <- vector("list", 365)
#running the code for 365 days, as each day has different routing matrix
for(i in 1:365){
  #define the size of the covariance matrix, based on the number of feasible paths (aka, number of columns) in the routing matrix 
  inx_cov <- ncol(rte_m[[i]])
  #generate covariance matrix (= unit matrix) for each day
  cov_m[[i]] <- diag(x=1, nrow = inx_cov, ncol = inx_cov)
  #call out routing matrix for each i-th day 
  rte_mat <- as.matrix(rte_m[[i]])
  #compute coefficient "K" in the formula of Kalman-Filter for each day, based on the formula on page 37 of the report. 
  kf_vect[[i]] <- cov_m[[i]]%*%t(rte_mat)%*%solve(rte_mat%*%cov_m[[i]]%*%t(rte_mat) + cov_m[[i]])
  print(i)
  print(kf_vect[[i]][nrow(kf_vect[[i]]),])
}

#define lists to store error vector epsilon (= x - A*dh) and historical demand dh vector
eps_vect <- vector("list", 365)
dh_vect <- vector("list", 365)
#create lists to store random *vector* x-A*dh, wherethe currently observed ON/OFF for *all* the routes taken (aka, "x" vector) is not available to us. 
#We assume the currently observed ON/OFF is not very far different from the historical ON/OFF (= A*d_h) at each stop per each day
for(i in 1:365){
  eps_vect[[i]] <- sample.int(10, nrow(kf_vect[[i]]), replace=TRUE)
}

#install package "limSolve" which contains the "nnls" function to solve for the least-square solutions to Ax = b
install.packages("limSolve")
library(limSolve)
#create lists to store historical demand (dh) vector for each day
inx_dh = 0
#recover historical demand dh by solving the inconsistent equation A*dh=x with least-square method. Solver package is "nnls"
for(j in unique(d_zone_ON$Date)){
  #update the index of each "vector" element in a list dh_vect
  inx_dh <- inx_dh+1
  #obtain the row index of the first and last elements in the "Date" column that are equal to a fixed date "j"
  lb <- min(which(d_zone_ON$Date == j)) 
  ub <- max(which(d_zone_ON$Date == j)) 
  #the system of linear equation here is singular!
  dh_vect[[inx_dh]] <- nnls(rte_m[[inx_dh]], d_zone_ON$ON[lb:ub]) #recover historical demand d_h here
  print(j) #check number of iterations
}


#mean and variance "list" storage for Kalman-Filter result for each of 365 days
mean_kf <- vector("list", 365)
cov_kf  <- vector("list", 365)
#Compute the mean and variance vector of Kalman-Filter for each of the 365 days, using equations (2) and (4) on page 37 & 38 in the report. 
for(i in 1:365){
  #compute mean of kalman filter with formula: at a given time t, K*(x-A*d_h) + d_h 
  mean_kf[[i]] <- as.matrix(kf_vect[[i]])%*%as.matrix(eps_vect[[i]]) + as.vector(dh_vect[[i]]$X)
  row_mat <- nrow(as.matrix(kf_vect[[i]])%*%as.matrix(rte_m[[i]]))
  col_mat <- ncol(as.matrix(kf_vect[[i]])%*%as.matrix(rte_m[[i]]))
  #compute covariance matrix with formula: (I-K*A)*(sigma_2)^2
  cov_kf[[i]] <- (diag(x=1, nrow = row_mat, ncol = col_mat) - as.matrix(kf_vect[[i]])%*%as.matrix(rte_m[[i]]))%*%as.matrix(cov_m[[i]])
  #keep track of the calculations per loop.
  print(mean_kf[[i]][nrow(mean_kf[[i]]),])
  print(cov_kf[[i]][nrow(cov_kf[[i]]),])
  print(i)
}
#perform exploratory data analysis by aggregating ON counts data based on Date, Days of Week and month 
d1_ON = d%>% group_by(Date) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ON_Mth = d%>%  group_by(month) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ON_AVG = d%>% group_by(Date) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_ON_DOW = d%>% group_by(DOW) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ON_DOWAVG = d%>% group_by(DOW) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_ON_MthAVG = d%>%  group_by(month) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
#order the dataset in increasing order based on Date
d1_ON <- d1_ON[order(d1_ON$Date, decreasing=FALSE),]
#create a month column for data frame d1_ON
d1_ON$mth <- substr(d1_ON$Date,4,6)
#create a subset of d1_ON with only 3 columns: 'Date', 'amount of data in each date' and 'total ON counts'
d1_ON <- subset(d1_ON, select = c("Date", "n", "sum"))

#aggregate ON/OFF counts per each route.
d2_ON = d %>% group_by(Route) %>% summarize (n=n(), sum_on = sum(as.numeric(ON)), sum_off = sum(as.numeric(OFF)))
#Create a new data frame with only 2 columns: Route and total ON counts
d2_new <- d2_ON[c('Route', 'sum_on')]

#aggregate mean ON counts per each route
d2_ON_AVG = d %>% group_by(Route) %>% summarize (n=n(), mean = mean(as.numeric(ON)))

#create total and average ON count per days of week
par(mfrow = c(2,1))
ON_DOW <- d1_ON_DOW$sum
ON_AVG <- d1_ON_AVG$mean
ON_Mth <- d1_ON_Mth$sum
ON_MthAVG <- d1_ON_MthAVG$mean
Dayofyear <- d1_ON$Date
#displaying labels on x-axis as Monday - Sunday
Dayofweek <- factor(d1_ON_DOW$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
#displaying month as JAN-DEC
Month <- factor(d1_ON_Mth$month, levels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))

#generate bar plots for total and average APC ON per day of week, per month
ggplot(d1_ON_DOW,aes(y= ON_DOW, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total APC ON per day of a week")
ggplot(d1_ON_AVG,aes(y= ON_AVG, x = Dayofyear)) + geom_bar(stat="identity") + ggtitle("Average APC ON per each day")
ggplot(d1_ON_Mth,aes(y= ON_Mth, x = Month)) + geom_bar(stat="identity") + ggtitle("Total APC ON per each month")
ggplot(d1_ON_MthAVG,aes(y= ON_MthAVG, x = Month)) + geom_bar(stat="identity") + ggtitle("Average APC ON per each month")

#generate bar plot for total APC ON per day of year
ON <- d1_ON$sum
ggplot(d1_ON,aes(y= ON, x = Dayofyear)) + geom_bar(stat="identity") + ggtitle("Total APC ON per day of year 2015")

#From this line is the data exploratory analysis on one-month APC dataset
stops = d %>% select(Lat,Long) %>% distinct()

#visualize the bus stops on the google map
sbbox <- make_bbox(lon = stops$Long, lat = stops$Lat, f = .05)
sq_map <- get_map(location = sbbox, source = "google")
ggmap(sq_map) + geom_point(data = stops, mapping = aes(x = Long, y = Lat), color = "red")

#exploring dataset
d %>% select(GeoNodeId,Lat,Long) %>% distinct() %>% count()
d %>% select(GeoNodeId) %>% distinct() %>% count()
d %>% select(Stop.Name, Lat,Long) %>% distinct() %>% count()
d %>% select(Stop.Name) %>% distinct() %>% count()
d %>% select(BusID) %>% distinct() %>% count()
d %>% select(Block) %>% distinct() %>% count()
d %>% select(Route) %>% distinct() %>% count()
d %>% select(Run) %>% distinct() %>% count()
d %>% select(TripTime) %>% distinct() %>% count()

#group data into bus stop based on geonodeID, lat and long
stops = d %>% select(GeoNodeId,Lat,Long) %>% distinct()

#visualize the locations of these bus stops
sbbox <- make_bbox(lon = stops$Long, lat = stops$Lat, f = .1)
sq_map <- get_map(location = sbbox, source = "google")
ggmap(sq_map) + geom_point(data = stops, mapping = aes(x = Long, y = Lat), color = "red")

#compute total ON and OFF at each of these bus stops
sum(d$ON)
sum(d$OFF)

#convert ON/OFF data from char to numeric type
d$ON <- as.numeric(d$ON)
d$OFF <- as.numeric(d$OFF)
#compute average ON count data per days of week
by_dow  = d %>% group_by(DOW) %>% summarize(n = n(), mean = mean(ON))
#c
by_dow$DOW <- factor(by_dow$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
#compute average OFF count data per days of week and re-order the days of week from Mon-Sun
by_dow2  = d %>% group_by(DOW) %>% summarize(n = n(), mean = mean(OFF))
by_dow2$DOW <- factor(by_dow2$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

#compute total ON count data per days of week and re-order the days of week from Mon-Sun
by_dow_sum = d %>% group_by(DOW) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum$DOW <- factor(by_dow_sum$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

#compute total OFF count data per days of week and re-order the days of week from Mon-Sun
by_dow_sum2 = d %>% group_by(DOW) %>% summarize(n = n(), sum = sum(OFF))
by_dow_sum2$DOW <- factor(by_dow_sum2$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

#compute average ON count data per days of week and re-order the days of week from Mon-Sun
par(mfrow = c(2,1))
Avg_APCON <- by_dow$mean
Dayofweek <- by_dow$DOW
#create the bar plot for the average APC ON per days of the week
ggplot(by_dow,aes(y= Avg_APCON, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Average APC ON per day of a week")

#create the bar plot for the average APC OFF per days of week
Avg_APCOff <- by_dow2$mean
Dayofweek <- by_dow2$DOW
ggplot(by_dow2, aes(y= Avg_APCOff, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Average APC Off per day of a week")

#order data frame d by days of week and hour
d2 <- d[order(d$Date, d$DOW, substr(d$Trip.Time,1,2)),]
#compute total and mean ON/OFF counts per hour
by_dow3  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(ON))
by_dow4  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(OFF))
by_dow_sum3  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum4  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(OFF))

#Order the data frame by Date
d3 <- d[order(d$Date),]
#compute total and mean ON/OFF counts data per Day
by_dow5 = d3 %>% group_by(Date) %>% summarize(n = n(), mean = mean(ON))
by_dow6  = d3 %>% group_by(Date) %>% summarize(n = n(), mean = mean(OFF))
by_dow_sum5 = d3 %>% group_by(Date) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum6 = d3 %>% group_by(Date) %>% summarize(n = n(), sum = sum(OFF))

#create two bar plots of average APC ON/OFF across hours of a day.
par(mfrow = c(2,1))
barplot(by_dow3$mean, main = "Average APC ON by hour of the day", names.arg = sort(c(unique(substr(d$Trip.Time,1,2)))), decreasing = FALSE, ylab = "Mean APC ON", las= 2)
barplot(by_dow4$mean, main = "Average APC OFF by hour of the day",names.arg = sort(c(unique(substr(d$Trip.Time,1,2))), decreasing = FALSE), ylab = "Mean APC OFF", las=2)

#create two bar plots of average APC ON/OFF per each day of a month
par(mfrow = c(2,1))
barplot(by_dow5$mean, main = "Average APC ON by each day of the month", names.arg = c(1:31), ylab = "Mean APC ON", las = 3)
barplot(by_dow6$mean, main = "Average APC OFF by each day of the month", names.arg = c(1:31), ylab = "Mean APC OFF", las = 3)

#aggregate ON/OFF count from different data frames
Sum_APCON <- by_dow_sum3$sum
Hour <- unique(by_dow_sum3$`substr(d$Trip.Time, 1, 2)`)
Sum_APCOFF <- by_dow_sum4$sum
SUM_APCON_Month <-by_dow_sum5$sum
SUM_APCOFF_Month <- by_dow_sum6$sum

#create two bar plots for total APC ON/OFF per hours of a day
par(mfrow = c(2,1))
ggplot(by_dow_sum3,aes(y= Sum_APCON, x = Hour)) + geom_bar(stat="identity") + ggtitle("Total APC ON per hour of a day")
ggplot(by_dow_sum4,aes(y= Sum_APCOFF, x = Hour)) + geom_bar(stat="identity") + ggtitle("Total APC OFF per hour of a day")

#create two bar plots for total APC ON/OFF per days of month
par(mfrow = c(2,1))
barplot(by_dow_sum5$sum, main = "Total APC ON by each day of the month", names.arg = c(1:31), ylab = "Total APC ON", las = 3)
barplot(by_dow_sum6$sum, main = "Total APC OFF by each day of the month", names.arg = c(1:31), ylab = "Total APC OFF", las = 3)

#compute total and mean ON/OFF count data per route for each day in a month
d4 <- d[order(d$Date, decreasing=FALSE),]
by_dow7  = d4 %>% group_by(Route,month, Date) %>% summarize(n = n(), mean = mean(ON))
by_dow8  = d4 %>% group_by(Route,month, Date) %>% summarize(n = n(), mean = mean(OFF))
by_dow_sum7 =  d4 %>% group_by(Route,month, Date) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum8 =  d4 %>% group_by(Route, month, Date) %>% summarize(n = n(), sum = sum(OFF))

#compute total and mean ON/OFF count data per hour
by_dow9 = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(ON))
by_dow10  = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(OFF))
by_dow_sum9 = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum10  = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(OFF))

#compute total and mean ON/OFF count data per days of a week
by_dow11 = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), mean = mean(ON))
by_dow12  = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), mean = mean(OFF))
by_dow_sum11 = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum12  = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), sum = sum(OFF))

#create bar plot for mean ON/OFF for route "208" per each day of October 2017
par(mfrow = c(2,1))
newdata <- subset(by_dow7, by_dow7$Route == "208")
Day <- substr(newdata$Date,1,2)
APCON_R208 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route208 per each day of October 2017")
newdata <- subset(by_dow8, by_dow8$Route == "208")
Day <- substr(newdata$Date,1,2)
APCoff_R208 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route208 per each day of October 2017")

#create all the bar plots for the total APC ON per each specific route for each day it's available in 2015
plot_list = list()
for (i in unique(by_dow_sum7$Route)) {
  newdata <- subset(by_dow_sum7, by_dow_sum7$Route == as.character(i))
  Day <- substr(newdata$Date,1,2)
  APCON <- newdata$sum
  p = ggplot(newdata,aes(x = Day, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Total APCON-Route", i, "per days of 2015"))
  plot_list[[i]] = p
  file_name = paste("Total APCON-Route", i, "per days of 2015.jpg")
  jpeg(file_name)
  print(plot_list[[i]])
  dev.off()
}  

#create all the bar plots for the total APC OFF per each specific route for each day it's available in 2015
plot_list2 = list()
for (i in unique(by_dow_sum8$Route)) {
  newdata <- subset(by_dow_sum8, by_dow_sum8$Route == as.character(i))
  Day <- substr(newdata$Date,1,2)
  APCOFF <- newdata$sum
  p = ggplot(newdata,aes(x = Day, y = APCOFF)) + geom_bar(stat="identity") + ggtitle(paste("Total APCOFF-Route", i, "per each day of October 2017"))
  plot_list2[[i]] = p
  file_name = paste("Total APCOFF-Route", i, "per each day of October 2017.jpg")
  jpeg(file_name)
  print(plot_list2[[i]])
  dev.off()
}  

#create all the bar plots for the total APC OFF in a specific route per hours of a day 
plot_list3 = list()
for (i in unique(by_dow_sum9$Route)) {
  newdata <- subset(by_dow_sum9, by_dow_sum9$Route == as.character(i))
  Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
  APCON <- newdata$sum
  p = ggplot(newdata,aes(x = Time, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Total APCON-Route", i, "per each hour of a day"))
  plot_list3[[i]] = p
  file_name = paste("Total APCON-Route", i, "per each hour of a day.jpg")
  jpeg(file_name)
  print(plot_list3[[i]])
  dev.off()
}  

#create all the bar plots for the total APC OFF in a specific route per hours of a day 
plot_list4 = list()
for (i in unique(by_dow_sum10$Route)) {
  newdata <- subset(by_dow_sum10, by_dow_sum10$Route == as.character(i))
  Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
  APCOFF <- newdata$sum
  p = ggplot(newdata,aes(x = Time, y = APCOFF)) + geom_bar(stat="identity") + ggtitle(paste("Total APCOFF-Route", i, "per each hour of a day"))
  plot_list4[[i]] = p
  file_name = paste("Total APCOFF-Route", i, "per each hour of a day.jpg")
  jpeg(file_name)
  print(plot_list4[[i]])
  dev.off()
}  

#create all the bar plots for the total APC ON in a specific route per days of a week
plot_list5 = list()
for (i in unique(by_dow_sum11$Route)) {
  newdata <- subset(by_dow_sum11, by_dow_sum11$Route == as.character(i))
  Dayofweek <-factor(newdata$`d4$DOW`, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
  APCON <- newdata$sum
  p = ggplot(newdata,aes(x = Dayofweek, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Total APCON-Route", i, "per each day of a week"))
  plot_list5[[i]] = p
  file_name = paste("Total APCON-Route", i, "per each day of a week.jpg")
  jpeg(file_name)
  print(plot_list5[[i]])
  dev.off()
}  
#create all the bar plots for the total APC OFF in a specific route per days of a week
plot_list6 = list()
for (i in unique(by_dow_sum12$Route)) {
  newdata <- subset(by_dow_sum12, by_dow_sum12$Route == as.character(i))
  Dayofweek <-factor(newdata$`d4$DOW`, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
  APCOFF <- newdata$sum
  p = ggplot(newdata,aes(x = Dayofweek, y = APCOFF)) + geom_bar(stat="identity") + ggtitle(paste("Total APCOFF-Route", i, "per each day of a week"))
  plot_list6[[i]] = p
  file_name = paste("Total APCOFF-Route", i, "per each day of a week.jpg")
  jpeg(file_name)
  print(plot_list6[[i]])
  dev.off()
}  
#From this to line 687 - create bar plots average APC ON/OFF per route i per each day of the month
newdata <- subset(by_dow7, by_dow7$Route == "209")
Day <- substr(newdata$Date,1,2)
APCON_R209 <- newdata$mean

ggplot(newdata,aes(x = Day, y = APCON_R209)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route209 per each day of October 2017")
newdata <- subset(by_dow8, by_dow8$Route == "209")
Day <- substr(newdata$Date,1,2)
APCoff_R209 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R209)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route209 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "210")
Day <- substr(newdata$Date,1,2)
APCON_R210 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R210)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route210 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "210")
Day <- substr(newdata$Date,1,2)
APCoff_R210 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R210)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route210 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "215")
Day <- substr(newdata$Date,1,2)
APCON_R215 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R215)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route215 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "215")
Day <- substr(newdata$Date,1,2)
APCoff_R215 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R215)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route215 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "221")
Day <- substr(newdata$Date,1,2)
APCON_R221 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R221)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route221 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "221")
Day <- substr(newdata$Date,1,2)
APCoff_R221 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R221)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route221 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "223")
Day <- substr(newdata$Date,1,2)
APCON_R223 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R223)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route223 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "223")
Day <- substr(newdata$Date,1,2)
APCoff_R223 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R223)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route223 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "225")
Day <- substr(newdata$Date,1,2)
APCON_R225 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R225)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route225 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "225")
Day <- substr(newdata$Date,1,2)
APCoff_R225 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R225)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route225 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "226")
Day <- substr(newdata$Date,1,2)
APCON_R226 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R226)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route226 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "226")
Day <- substr(newdata$Date,1,2)
APCoff_R226 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R226)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route226 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "230")
Day <- substr(newdata$Date,1,2)
APCON_R230 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R230)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route230 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "230")
Day <- substr(newdata$Date,1,2)
APCoff_R230 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R230)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route230 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "234")
Day <- substr(newdata$Date,1,2)
APCON_R234 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R234)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route234 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "234")
Day <- substr(newdata$Date,1,2)
APCoff_R234 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R234)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route234 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "237")
Day <- substr(newdata$Date,1,2)
APCON_R237 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R237)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route237 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "237")
Day <- substr(newdata$Date,1,2)
APCoff_R237 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R237)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route237 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "240")
Day <- substr(newdata$Date,1,2)
APCON_R240 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R240)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route240 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "240")
Day <- substr(newdata$Date,1,2)
APCoff_R240 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R240)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route240 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "241")
Day <- substr(newdata$Date,1,2)
APCON_R241 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R241)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route241 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "241")
Day <- substr(newdata$Date,1,2)
APCoff_R241 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R241)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route241 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "250")
Day <- substr(newdata$Date,1,2)
APCON_R250 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R250)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route250 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "250")
Day <- substr(newdata$Date,1,2)
APCoff_R250 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R250)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route250 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "270")
Day <- substr(newdata$Date,1,2)
APCON_R270 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R270)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route270 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "270")
Day <- substr(newdata$Date,1,2)
APCoff_R270 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R270)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route270 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "272")
Day <- substr(newdata$Date,1,2)
APCON_R272 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R272)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route272 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "272")
Day <- substr(newdata$Date,1,2)
APCoff_R272 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R272)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route272 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "290")
Day <- substr(newdata$Date,1,2)
APCON_R290 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R290)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route290 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "290")
Day <- substr(newdata$Date,1,2)
APCoff_R290 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R290)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route290 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "326")
Day <- substr(newdata$Date,1,2)
APCON_R326 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R326)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route326 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "326")
Day <- substr(newdata$Date,1,2)
APCoff_R326 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R326)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route326 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "422")
Day <- substr(newdata$Date,1,2)
APCON_R422 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R422)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route422 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "422")
Day <- substr(newdata$Date,1,2)
APCoff_R422 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R422)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route422 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "423")
Day <- substr(newdata$Date,1,2)
APCON_R423 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R423)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route423 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "423")
Day <- substr(newdata$Date,1,2)
APCoff_R423 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R423)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route423 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "600")
Day <- substr(newdata$Date,1,2)
APCON_R600 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R600)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route600 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "600")
Day <- substr(newdata$Date,1,2)
APCoff_R600 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R600)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route600 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "606")
Day <- substr(newdata$Date,1,2)
APCON_R606 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R606)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route606 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "606")
Day <- substr(newdata$Date,1,2)
APCoff_R606 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R606)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route606 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "610")
Day <- substr(newdata$Date,1,2)
APCON_R610 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R610)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route610 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "610")
Day <- substr(newdata$Date,1,2)
APCoff_R610 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R610)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route610 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "616")
Day <- substr(newdata$Date,1,2)
APCON_R616 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R616)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route616 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "616")
Day <- substr(newdata$Date,1,2)
APCoff_R616 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R616)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route616 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "694")
Day <- substr(newdata$Date,1,2)
APCON_R694 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R694)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route694 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "694")
Day <- substr(newdata$Date,1,2)
APCoff_R694 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R694)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route694 per each day of October 2017")

newdata <- subset(by_dow7, by_dow7$Route == "696")
Day <- substr(newdata$Date,1,2)
APCON_R696 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R696)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route696 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "696")
Day <- substr(newdata$Date,1,2)
APCoff_R696 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R696)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route696 per each day of October 2017")

#create all the bar plots for average APC ON in a specific route "i" per hours of a day
plot_list7 = list()
for (i in unique(by_dow9$Route)) {
  newdata <- subset(by_dow9, by_dow9$Route == as.character(i))
  Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
  APCON <- newdata$mean
  p = ggplot(newdata,aes(x = Time, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Average APCON-Route", i, "per each hour of a day"))
  plot_list7[[i]] = p
  file_name = paste("Average APCON-Route", i, "per each hour of a day.jpg")
  jpeg(file_name)
  print(plot_list7[[i]])
  dev.off()
} 

plot_list8 = list()
for (i in unique(by_dow11$Route)) {
  newdata <- subset(by_dow11, by_dow11$Route == as.character(i))
  # Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
  # APCOFF <- newdata$mean
  Dayofweek <-factor(newdata$`d4$DOW`, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
  p = ggplot(newdata,aes(x = Dayofweek, y = mean)) + geom_bar(stat="identity") + ggtitle(paste("Average APCON-Route", i, "per day of a week"))
  plot_list8[[i]] = p
  file_name = paste("Average APCON-Route", i, "per day of a week.jpg")
  jpeg(file_name)
  print(plot_list8[[i]])
  dev.off()
}  

#From here to line 1238 - create average ON/OFF for each route i across different hours of a day
newdata <- subset(by_dow9, by_dow9$Route == "208")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R208 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route208 per each time of a day")

newdata <- subset(by_dow10, by_dow8$Route == "208")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R208 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route208 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "209")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R209 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R209)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route209 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "209")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R209 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R209)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route209 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "210")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R210 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R210)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route210 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "210")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R210 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R210)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route210 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "215")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R215 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R215)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route215 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "215")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R215 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R215)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route215 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "221")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R221 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R221)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route221 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "221")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R221 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R221)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route221 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "223")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R223 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R223)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route223 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "223")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R223 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R223)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route223 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "225")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R225 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R225)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route225 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "225")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R225 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R225)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route225 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "226")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R226 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R226)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route226 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "226")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R226 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R226)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route226 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "230")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R230 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R230)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route230 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "230")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R230 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R230)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route230 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "234")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R234 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R234)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route234 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "234")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R234 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R234)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route234 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "237")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R237 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R237)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route237 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "237")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R237 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R237)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route237 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "240")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R240 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R240)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route240 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "240")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R240 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R240)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route240 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "241")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R241 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R241)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route241 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "241")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R241 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R241)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route241 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "250")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R250 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R250)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route250 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "250")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R250 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R250)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route250 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "270")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R270 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R270)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route270 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "270")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R270 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R270)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route270 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "272")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R272 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R272)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route272 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "272")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R272 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R272)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route272 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "290")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R290 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R290)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route290 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "290")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R290 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R290)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route290 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "326")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R326 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R326)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route326 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "326")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R326 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R326)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route326 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "422")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R422 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R422)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route422 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "422")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R422 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R422)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route422 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "423")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R423 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R423)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route423 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "423")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R423 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R423)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route423 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "600")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R600 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R600)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route600 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "600")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R600 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R600)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route600 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "606")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R606 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R606)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route606 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "606")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R606 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R606)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route606 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "610")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R610 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R610)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route610 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "610")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R610 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R610)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route610 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "616")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R616 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R616)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route616 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "616")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R616 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R616)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route616 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "694")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R694 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R694)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route694 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "694")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R694 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R694)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route694 per each time of a day")

newdata <- subset(by_dow9, by_dow9$Route == "696")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCON_R696 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCON_R696)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route696 per each time of a day")

newdata <- subset(by_dow10, by_dow10$Route == "696")
Time <- (newdata$`substr(d4$Trip.Time, 1, 2)`)
APCoff_R696 <- newdata$mean
ggplot(newdata,aes(x = Time, y = APCoff_R696)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route696 per each time of a day")

#Average APC ON/OFF per each day of a week
newdata <- subset(by_dow11, by_dow11$Route == "208")
Day <- newdata$`d4$DOW`
APCON_R208 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route208 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "208")
Day <- newdata$`d4$DOW`
APCoff_R208 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route208 per each day of a week")


newdata <- subset(by_dow11, by_dow11$Route == "209")
Day <- newdata$`d4$DOW`
APCON_R209 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R209)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route209 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "209")
Day <- newdata$`d4$DOW`
APCoff_R209 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R209)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route209 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "210")
Day <- newdata$`d4$DOW`
APCON_R210 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R210)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route210 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "210")
Day <- newdata$`d4$DOW`
APCoff_R210 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R210)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route210 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "215")
Day <- newdata$`d4$DOW`
APCON_R215 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R215)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route215 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "215")
Day <- newdata$`d4$DOW`
APCoff_R215 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R215)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route215 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "221")
Day <- newdata$`d4$DOW`
APCON_R221 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R221)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route221 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "221")
Day <- newdata$`d4$DOW`
APCoff_R221 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R221)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route221 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "223")
Day <- newdata$`d4$DOW`
APCON_R223 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R223)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route223 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "223")
Day <- newdata$`d4$DOW`
APCoff_R223 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R223)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route223 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "225")
Day <- newdata$`d4$DOW`
APCON_R225 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R225)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route225 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "225")
Day <- newdata$`d4$DOW`
APCoff_R225 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R225)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route225 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "226")
Day <- newdata$`d4$DOW`
APCON_R226 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R226)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route226 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "226")
Day <- newdata$`d4$DOW`
APCoff_R226 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R226)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route226 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "230")
Day <- newdata$`d4$DOW`
APCON_R230 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R230)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route230 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "230")
Day <- newdata$`d4$DOW`
APCoff_R230 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R230)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route230 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "234")
Day <- newdata$`d4$DOW`
APCON_R234 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R234)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route234 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "234")
Day <- newdata$`d4$DOW`
APCoff_R234 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R234)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route234 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "237")
Day <- newdata$`d4$DOW`
APCON_R237 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R237)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route237 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "237")
Day <- newdata$`d4$DOW`
APCoff_R237 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R237)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route237 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "240")
Day <- newdata$`d4$DOW`
APCON_R240 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R240)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route240 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "240")
Day <- newdata$`d4$DOW`
APCoff_R240 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R240)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route240 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "241")
Day <- newdata$`d4$DOW`
APCON_R241 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R241)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route241 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "241")
Day <- newdata$`d4$DOW`
APCoff_R241 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R241)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route241 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "250")
Day <- newdata$`d4$DOW`
APCON_R250 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R250)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route250 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "250")
Day <- newdata$`d4$DOW`
APCoff_R250 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R250)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route250 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "270")
Day <- newdata$`d4$DOW`
APCON_R270 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R270)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route270 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "270")
Day <- newdata$`d4$DOW`
APCoff_R270 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R270)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route270 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "272")
Day <- newdata$`d4$DOW`
APCON_R272 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R272)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route272 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "272")
Day <- newdata$`d4$DOW`
APCoff_R272 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R272)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route272 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "290")
Day <- newdata$`d4$DOW`
APCON_R290 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R290)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route290 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "290")
Day <- newdata$`d4$DOW`
APCoff_R290 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R290)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route290 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "326")
Day <- newdata$`d4$DOW`
APCON_R326 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R326)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route326 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "326")
Day <- newdata$`d4$DOW`
APCoff_R326 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R326)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route326 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "422")
Day <- newdata$`d4$DOW`
APCON_R422 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R422)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route422 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "422")
Day <- newdata$`d4$DOW`
APCoff_R422 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R422)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route422 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "423")
Day <- newdata$`d4$DOW`
APCON_R423 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R423)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route423 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "423")
Day <- newdata$`d4$DOW`
APCoff_R423 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R423)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route423 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "600")
Day <- newdata$`d4$DOW`
APCON_R600 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R600)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route600 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "600")
Day <- newdata$`d4$DOW`
APCoff_R600 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R600)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route600 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "606")
Day <- newdata$`d4$DOW`
APCON_R606 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R606)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route606 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "606")
Day <- newdata$`d4$DOW`
APCoff_R606 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R606)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route606 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "610")
Day <- newdata$`d4$DOW`
APCON_R610 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R610)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route610 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "610")
Day <- newdata$`d4$DOW`
APCoff_R610 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R610)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route610 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "616")
Day <- newdata$`d4$DOW`
APCON_R616 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R616)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route616 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "616")
Day <- newdata$`d4$DOW`
APCoff_R616 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R616)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route616 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "694")
Day <- newdata$`d4$DOW`
APCON_R694 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R694)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route694 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "694")
Day <- newdata$`d4$DOW`
APCoff_R694 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R694)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route694 per each day of a week")

newdata <- subset(by_dow11, by_dow11$Route == "696")
Day <- newdata$`d4$DOW`
APCON_R696 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R696)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route696 per each day of a week")

newdata <- subset(by_dow12, by_dow12$Route == "696")
Day <- newdata$`d4$DOW`
APCoff_R696 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R696)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route696 per each day of a week")


#create two bar plots for total APC ON/OFF per days of a week
Sum_APCON <- by_dow_sum$sum
Dayofweek <- by_dow_sum$DOW
Sum_APCOFF <- by_dow_sum2$sum
Dayofweek <- by_dow_sum2$DOW
par(mfrow = c(2,1))
ggplot(by_dow,aes(y= Sum_APCON, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total APC ON per day of a week")
ggplot(by_dow,aes(y= Sum_APCOFF, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total APC OFF per day of a week")

#install package ggmap and gridextra
install.packages("ggmap", type = "source")
install.packages("gridExtra")
library(ggmap)
library(gridExtra)
library(scales)

#create heatmap
qmap('chicago', zoom = 13)
dataset2 <- subset(d, 0 < d$ON | 0 < d$OFF)
keep <- c("Date", "DOW", "Block", "Route", "Direction","Trip.ID","Stop.Name","ON","OFF","Lat","Long")
dataset2 <- subset(dataset2 , select = keep)
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")

require('ggmap')
#create a map where heat is shown
map.in <- get_map(location = c(min(d_zone$Long),
                               min(d_zone$Lat),
                               max(d_zone$Long),
                               max(d_zone$Lat)),
                  source = "google")
ggmap(map.in) + geom_point(data = stops, mapping = aes(x = lon, y = lat), color = "red")


theme_set(theme_bw(base_size = 8))
colormap <- c("Violet","Blue","Green","Yellow","Red","White")
d$DOW <- factor(d$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
dataset2$DOW <- factor(dataset2$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

#compute average and total ON/OFF counts per time variations
d1_ONzone = d_zone%>% group_by(DOW, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ONAVG = d_zone %>% group_by(DOW) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_OFFzone = d_zone%>% group_by(DOW, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(OFF)))
d1_OFFAVG = d_zone %>% group_by(DOW, Long, Lat) %>% summarize(n=n(), mean = mean(as.numeric(OFF)))
d_zone$month <- substr(d_zone$Date,4,6)
d1_ONzone_month = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_OFFzone_month = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(OFF)))
d1_ONzone_monthmean = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_OFFzone_monthmean = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), mean = mean(as.numeric(OFF)))

#create a heat map for average APC OFF per month
pred.stat.map.final <- ggmap(map.in) %+% d1_OFFzone_monthmean + 
  aes(x = Long,
      y = Lat,
      z = mean) +
  stat_summary_2d(binwidth = c(.1, .1),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "mean",
                       colours =  YlOrBr,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of Average APC OFF per month") + facet_wrap(~ month) + 
  coord_map()
print(pred.stat.map.final)

#create a heat map for average APC OFF per month with new dataset that does not contain "0" count data for OFF
pred.stat.map.final2 <- ggmap(map.in2) %+% dataset2 + 
  aes(x = Long,
      y = Lat,
      z = OFF) +
  stat_summary_2d(fun = mean, 
                  binwidth = c(.1, .1),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Mean",
                       colours =  YlOrBr,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude", title = "Heat map of Average APC ON (none zero) per day of week") + facet_wrap(~ DOW) + 
  coord_map()
print(pred.stat.map.final2)





