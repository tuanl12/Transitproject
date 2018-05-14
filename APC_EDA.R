#install all essential packages
install.packages("tidyverse")
install.packages("hash")
install.packages("ggmap")
install.packages("ddply")
install.packages("ggproto")
install.packages("chron")
install.packages("scales")

#load all these libraries just in case!
library(hash)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(lubridate)
library(xts)
library(chron)
library(dplyr)

#create an APC dataset with column headers and data types in each column.
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

#read all the data files with the filename ending in "2015.txt" and combine it into the data frame called apc.
fl=list.files(".", pattern="2015.txt")
for (fn in fl) {
  print(fn)
  d = read_delim(fn, col_names = F, delim = ',',
                   col_types = cols(.default = "c"))
  apc = rbind(apc,d[,1:16])
}

#rename all the columns' names for easy interpretation and data manipulation
apc = dplyr::rename(apc, Date = X1 , DOW = X2, Block = X3, Run = X4, Route = X5, Direction = X6, Trip_ID = X7, 
                       order_numb = X8,  Stop_Name = X9, Stop_Sequence = X10, Lat = X11, Long = X12,  ON = X13, OFF = X14, Load = X15, BusID = X16)
d = apc
d$Date <- substr(d$Date,1,6)
d$month <- substr(d$Date,4,6)

d[is.na(d$ON)] <- 0
d[is.na(d$OFF)] <- 0
d <- d[complete.cases(d),]
d$Lat <- as.numeric(d$Lat)
d$Long <- as.numeric(d$Long)
d["X8"] <- NA

stops <- read_delim("/Users/tuanle/stop_zone.csv", delim = ",", quote = '', col_names = FALSE, col_types = cols(.default = "c"))
stops <- stops[, c("X2", "X3", "X5", "X6", "X13", "X14")]
stops$X5[2:length(stops$X5)] <- as.numeric(stops$X5[2:length(stops$X5)])
stops$X6[2:length(stops$X6)] <- as.numeric(stops$X6[2:length(stops$X6)])
stops = stops[-1,]
stops = dplyr::rename(stops, Lat="X5", Long="X6")
stops = dplyr::rename(stops, Stop_Name = "X3")

#tried left_join, but it does not work. Neither is dplyr::left_join, because d_zone has much larger size than d
d_zone <- left_join(x = d, y = stops%>% select("X13", "X14"), by = c("Stop_Sequence" ="X13"))

#column "X14" denote the zone
d_zone_ON = d_zone%>% group_by(month, Date, X14) %>% summarize(n=n(), ON = sum(as.numeric(ON)))
d_zone_OFF = d_zone%>% group_by(month, Date, X14) %>% summarize(n=n(), ON = sum(as.numeric(OFF)))
d_zone_ONmonth = d_zone_ON%>% group_by(month, X14) %>% summarize(n=n(), ON = sum(as.numeric(ON)))

#eliminate all of the rows with "NA" entries, except the rows corresponding to column "sum"
d_zone_ON[is.na(d_zone_ON$n)] <- 0
d_zone_ON <- d_zone_ON[complete.cases(d_zone_ON),]
d_zone_ONmonth[is.na(d_zone_ONmonth$n)] <- 0
d_zone_ONmonth <- d_zone_ONmonth[complete.cases(d_zone_ONmonth),]

# write.csv(d_zone_ON, file = "d_zone_ON.csv")
# write.csv(d_zone_ONmonth, file = "d_zone_ONmonth.csv")

rte_m <- vector("list", length(unique(d_zone_ON$Date)))
rte_m2 <- vector("list", length(unique(d_zone_ON$month)))
# inx_row_Apr <- sum(d_zone_ON$Date == "01-APR")
# rte_m[[1]] <- data.frame(matrix(0, nrow = inx_row_Apr, ncol = inx_row_Apr))
# for(i in 1:inx_row_Apr){
#   if(i==1){
#     rte_m[[1]][i,2:inx_row_Apr] = 1
#     rte_m[[1]][i,i] = 0;
#   }
#   else if(i<inx_row_Apr){
#       print(i);
#       rte_m[[1]][i,(i+1):inx_row_Apr] = 1;
#       rte_m[[1]][i,1:i] = 0;
#   } else {rte_m[[1]][i,] = 0}
# }
inx_big = 0
for(j in unique(d_zone_ON$Date)){
  inx_row <- sum(d_zone_ON$Date== j)
  inx_big <- inx_big+1
  rte_m[[inx_big]] <- data.frame(matrix(0, nrow = inx_row, ncol = inx_row))
  lb <- min(which(d_zone_ON$Date == j)) 
  ub <- max(which(d_zone_ON$Date == j)) 
  for(i in lb:ub){
    if(i==lb){
       rte_m[[inx_big]][1,2:(ub-lb+1)] = 1;
       rte_m[[inx_big]][1,1] = 0;
    }
    else if(i<ub && i> lb){
      rte_m[[inx_big]][i-lb+1, (i-lb+2):(ub-lb+1)] = 1;
      rte_m[[inx_big]][i-lb+1, 1:(i-lb+1)] = 0;
    } else {rte_m[[inx_big]][ub-lb+1, ] = 0}
  }
  print(j)
  print(nrow(rte_m[[inx_big]])) 
  print(ncol(rte_m[[inx_big]]))
}

inx_big2 = 0
for(j in unique(d_zone_ONmonth$month)){
  inx_row <- sum(d_zone_ONmonth$month== j)
  inx_big2 <- inx_big2+1
  rte_m2[[inx_big2]] <- data.frame(matrix(0, nrow = inx_row, ncol = inx_row))
  lb <- min(which(d_zone_ONmonth$month == j)) 
  ub <- max(which(d_zone_ONmonth$month == j)) 
  for(i in lb:ub){
    if(i==lb){
      rte_m2[[inx_big2]][1,2:(ub-lb+1)] = 1;
      rte_m2[[inx_big2]][1,1] = 0;
    }
    else if(i<ub && i> lb){
      rte_m2[[inx_big2]][i-lb+1, (i-lb+2):(ub-lb+1)] = 1;
      rte_m2[[inx_big2]][i-lb+1, 1:(i-lb+1)] = 0;
    } else {rte_m2[[inx_big2]][ub-lb+1, ] = 0}
  }
  print(j)
  print(nrow(rte_m2[[inx_big2]])) 
  print(ncol(rte_m2[[inx_big2]]))
}

#kalman_filter storage vector for 365 days in 2015 (first two) and 12 months (last two)
kf_vect <- vector("list", 365)
cov_m <- vector("list", 365)
kf_vect2 <- vector("list", 12)
cov_m2 <- vector("list", 12)
#365 days
for(i in 1:365){
  inx_cov <- nrow(rte_m[[i]])
  #generate covariance matrix for each day
  cov_m[[i]] <- diag(x=1, nrow = inx_cov, ncol = inx_cov)
  rte_mat <- as.matrix(rte_m[[i]])
  #compute coefficient K in the formula of Kalman-Filter for each day
  kf_vect[[i]] <- cov_m[[i]]%*%t(rte_mat)%*%solve(rte_mat%*%cov_m[[i]]%*%t(rte_mat) + cov_m[[i]])
  print(i)
  print(kf_vect[[i]][nrow(kf_vect[[i]]),])
}
#12-month
for(i in 1:12){
  inx_cov2 <- nrow(rte_m2[[i]])
  cov_m2[[i]] <- diag(x=1, nrow = inx_cov2, ncol = inx_cov2)
  rte_mat2 <- as.matrix(rte_m2[[i]])
  #compute coefficient K in the formula of Kalman-Filter for each month
  kf_vect2[[i]] <- cov_m2[[i]]%*%t(rte_mat2)%*%solve(rte_mat2%*%cov_m2[[i]]%*%t(rte_mat2) + cov_m2[[i]])
  print(i)
  print(kf_vect2[[i]][nrow(kf_vect2[[i]]),])
}

#define lists to store epsilon = d- A*dh and dh
eps_vect <- vector("list", 365)
dh_vect <- vector("list", 365)
eps_vect2 <- vector("list", 12)
dh_vect2 <- vector("list",12)
#create lists to store random *vector* d-A*dh
for(i in 1:365){
  eps_vect[[i]] <- sample.int(5, nrow(kf_vect[[i]]), replace=TRUE)
}
#create list to store random "vector" d-A*dh
for(i in 1:12){
  eps_vect2[[i]] <- sample.int(5, nrow(kf_vect2[[i]]), replace=TRUE)
}
install.packages("limSolve")
library(limSolve)
#create lists to store dh
inx_dh = 0
#recover dH from overdetermined equation A*dh=x using least-square
for(j in unique(d_zone_ON$Date)){
  inx_dh <- inx_dh+1
  lb <- min(which(d_zone_ON$Date == j)) 
  ub <- max(which(d_zone_ON$Date == j)) 
  #because the system of linear equation here is singular!
  dh_vect[[inx_dh]] <- nnls(rte_m[[inx_dh]], d_zone_ON$ON[lb:ub])
  #print(inx_dh)
  print(j)
}

#create lists to store dh2
inx_dh2 = 0
for(j in unique(d_zone_ONmonth$month)){
  inx_dh2 <- inx_dh2+1
  lb <- min(which(d_zone_ONmonth$month == j)) 
  ub <- max(which(d_zone_ONmonth$month == j)) 
  dh_vect2[[inx_dh2]] <- lsei(rte_m2[[inx_dh2]], d_zone_ONmonth$ON[lb:ub], type=2) #because the system of linear equation here is singular!
  #print(inx_dh)
  print(j)
}

#mean and co-variance of Kalman-Filter
mean_kf <- vector("list", 365)
cov_kf  <- vector("list", 365)
mean_kf2 <- vector("list", 12)
cov_kf2 <- vector("list", 12)
#365 days
for(i in 1:365){
  mean_kf[[i]] <- as.matrix(kf_vect[[i]])%*%as.matrix(eps_vect[[i]]) + as.vector(dh_vect[[i]]$X)
  row_mat <- nrow(as.matrix(kf_vect[[i]])%*%as.matrix(rte_m[[i]]))
  col_mat <- ncol(as.matrix(kf_vect[[i]])%*%as.matrix(rte_m[[i]]))
  cov_kf[[i]] <- (diag(x=1, nrow = row_mat, ncol = col_mat) - as.matrix(kf_vect[[i]])%*%as.matrix(rte_m[[i]]))%*%as.matrix(cov_m[[i]])
  print(mean_kf[[i]][nrow(mean_kf[[i]]),])
  print(cov_kf[[i]][nrow(cov_kf[[i]]),])
  print(i)
}
#12 months
for(i in 1:12){
  mean_kf2[[i]] <- as.matrix(kf_vect2[[i]])%*%as.matrix(eps_vect2[[i]]) + as.matrix(dh_vect2[[i]]$X)
  row_mat2 <- nrow(as.matrix(kf_vect2[[i]])%*%as.matrix(rte_m2[[i]]))
  col_mat2 <- ncol(as.matrix(kf_vect2[[i]])%*%as.matrix(rte_m2[[i]]))
  cov_kf2[[i]] <- (diag(x=1, nrow = row_mat2, ncol = col_mat2) - as.matrix(kf_vect2[[i]])%*%as.matrix(rte_m2[[i]]))%*%as.matrix(cov_m2[[i]])
  print(mean_kf2[[i]][nrow(mean_kf2[[i]]),])
  print(cov_kf2[[i]][nrow(cov_kf2[[i]]),])
  print(i)
}

library(rstan)
library(MASS)
#library(truncnorm)
library(bayesplot)
library(bridgesampling)
library(MCMCpack)
library(ggplot2)
library(coda)
library(clusterGeneration)
require(MASS)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# lambda <- array(sample(0:100, size = ncol, replace = TRUE),ncol);
# yH = rpois(n = ncol,lambda);
# yT_sim = yH +  rnorm(ncol,0.4*lambda,lambda/8);
# x <- as.vector(A%*%yT_sim + rnorm(nrow,mean = 0, sd = 5));
# sigma_x <- array(5, nrow);
# sigma_y <- array(15, ncol);

#data preparation for inputting into Stan
for(i in 1:15){
  nrow = nrow(rte_m[[i]]);
  ncol = ncol(rte_m[[i]]);
  A <- as.matrix(rte_m[[i]]);
  sigma_x <- as.vector(sample.int(10, nrow(kf_vect[[i]]), replace=TRUE))
  sigma_y <- as.vector(eps_vect[[i]])
  # sigma_x <- diag(1, nrow)
  # sigma_y <- diag(1, nrow)
  yH <- as.vector(dh_vect[[i]]$X);
  yT <- yH + as.vector(eps_vect[[i]]); 
  epsilon <- sample.int(15, nrow(kf_vect[[i]]), replace=TRUE)
  x <- as.vector(as.matrix(rte_m[[i]])%*%yT) + epsilon
  iterations = 100;
  #input data into a list called stan_data
  # stan_data = list(nrow = nrow, ncol = ncol,
  #                  yH = yH, 
  #                  x = x, epsilon = epsilon,
  #                  A = A, sigma_x = sigma_x, sigma_y = sigma_y);
  #input it into our Stan model file "stamodeling.stan"
  stanmodel1 <- stan_model(file = "stamodeling.stan",
                           model_name = "stanmodel1");
  
  #NUTS (No U-Turn) sampler to generate posterior distribution
  stanfit <- sampling(stanmodel1, cores = parallel::detectCores(), data = list(ncol = ncol,nrow = nrow,
                                              yH = yH, 
                                              x=x, epsilon = epsilon,
                                              A = A, sigma_x = sigma_x, sigma_y = sigma_y)
                      ,iter=iterations, chains = 3, control = list(max_treedepth=13));
  print(i)
  #extract the result and plot graph
  posterior_yTtheta <- as.array(stanfit)
  file_name10 <- paste("Comparison_KFnnls_Day", i, ".jpeg")
  jpeg(file_name10)
  plot(yH, col=1, type='l', ylab="d", xlab = "Zone index", ylim=c(0,max(yT))) 
  lines(colMeans(posterior_yTtheta[,3,]),col=2, type = 'l', ylab="d", xlab = "Zone index")
  lines(yT,col=3) 
  lines(mean_kf[[i]], col=4, type = 'l', ylab = "d")
  legend(x = "topleft",legend = c("prior", "recovered", "true", "Kalman-Filter"), col=1:4, lty=c(1,1,1,1))
  dev.off()
  
  # #samples vs.iteration plot
  # samp_plt <- mcmc_trace(posterior_yTtheta)
  # file_name7 = paste("Traceplot_Day", i)
  # jpeg(file_name7)
  # print(samp_plt)
  # dev.off()
  # #posteior of yT
  # dens <- mcmc_dens(posterior_yTtheta[,2,])
  # file_name8 = paste("Density Plot_Day", i)
  # jpeg(file_name8)
  # print(dens)
  # dev.off()
  
  #get a list of named components corresponding to model parameters
  # list_of_draws <- extract(stanfit)
  # print(names(list_of_draws))
  # head(list_of_draws$yT)
  # #create MCMC CI intervals for first and last 20 components of posterior yTtheta
  # color_scheme_set("red")
  # file_name2 = paste("First20comps_Day",i)
  # jpeg(file_name2)
  # first20_plt <- mcmc_intervals(colMeans(posterior_yTtheta[,,1:20]))
  # print(first20_plt)
  # dev.off()
  # 
  # file_name3 = paste("Last20comps_Day",i)
  # jpeg(file_name3)
  # last20_plt <- mcmc_intervals(colMeans(posterior_yTtheta[,,(nrow-19):nrow]))
  # print(last20_plt)
  # dev.off()
  
  #generate MCMC histogram for first and last 20 components of posterior yTtheta
  # color_scheme_set("green")
  # first20_comps <- mcmc_hist(colMeans(posterior_yTtheta[,,1:20]), binwidth = 1.00)
  # last20_comps <- mcmc_hist(colMeans(posterior_yTtheta[,,(nrow-19):nrow]), binwidth = 1.00)
  # #save the graphs into different jpeg files
  # file_name4 = paste("Histogram_First20comps_Day",i)
  # file_name5 = paste("Histogram_Last20comps_Day",i)
  # jpeg(file_name4)
  # print(first20_comps)
  # dev.off()
  # jpeg(file_name5)
  # print(last20_comps)
  # dev.off()
  
  fit_summary <- summary(stanfit)
  file_name6 = paste("resultsummary_KFnnls_ Day", i)
  jpeg(file_name6)
  print(fit_summary$summary)
  dev.off()
  
  color_scheme_set("mix-brightblue-gray")
  file_name = paste("Rhatnnls_histogram_Day", i)
  jpeg(file_name)
  rhats <- rhat(stanfit)
  rhat_plt <- mcmc_rhat_hist(rhats) + yaxis_text(hjust = 1) + ggtitle(paste("Rhatnnls_histogram_Day", i))
  print(rhat_plt)
  dev.off()
}
#12 months only
for(i in 1:12){
  nrow = nrow(rte_m2[[i]]);
  ncol = ncol(rte_m2[[i]]);
  A <- as.matrix(rte_m2[[i]]);
  sigma_x <- as.vector(sample.int(10, nrow(kf_vect2[[i]]), replace=TRUE))
  sigma_y <- as.vector(eps_vect2[[i]])
  yH <- as.vector(dh_vect2[[i]]$X);
  yT <- yH + as.vector(eps_vect2[[i]]); 
  epsilon <- sample.int(15, nrow(kf_vect2[[i]]), replace=TRUE)
  x <- as.vector(as.matrix(rte_m2[[i]])%*%yT) + epsilon
  iterations = 150;
  #input data into a list called stan_data
  stan_data = list(nrow = nrow, ncol = ncol,
                   yH = yH, 
                   x = x, epsilon = epsilon,
                   A = A, sigma_x = sigma_x, sigma_y = sigma_y);
  #input it into our Stan model file "stamodeling.stan"
  stanmodel1 <- stan_model(file = "stamodeling.stan",
                           model_name = "stanmodel1");
  
  #NUTS (No U-Turn) sampler to generate posterior distribution
  stanfit <- sampling(stanmodel1, data = list(ncol = ncol,nrow = nrow,
                                              yH = yH, 
                                              x=x, epsilon = epsilon,
                                              A = A, sigma_x = sigma_x, sigma_y = sigma_y)
                      ,iter=iterations, chains = 3, cores = 2, control = list(max_treedepth = 15));
  print(i)
  #extract the result and plot graph
  posterior_yTtheta <- as.array(stanfit)
  file_name10 <- paste("Comparison between recovered, prior and true value_Month", i, ".jpeg")
  jpeg(file_name10)
  plot(yH, col=1, type='l', ylab="d", xlab = "Zone index", ylim=c(0,max(yT))) 
  lines(colMeans(posterior_yTtheta[,2,]),col=2, type = 'l', ylab="d", xlab = "Zone index")
  lines(mean_kf2[[i]], col=4, type = 'l', ylab = "d")
  lines(yT,col=3) 
  legend(x = "topleft",legend = c("prior", "recovered", "true", "kalman filter"), col=1:4, lty=c(1,1,1,1))
  dev.off()
  
  # #samples vs.iteration plot
  # samp_plt <- mcmc_trace(posterior_yTtheta)
  # file_name7 = paste("Traceplot_Day", i)
  # jpeg(file_name7)
  # print(samp_plt)
  # dev.off()
  # #posteior of yT
  # dens <- mcmc_dens(posterior_yTtheta[,2,])
  # file_name8 = paste("Density Plot_Day", i)
  # jpeg(file_name8)
  # print(dens)
  # dev.off()
  
  #get a list of named components corresponding to model parameters
  list_of_draws <- extract(stanfit)
  print(names(list_of_draws))
  head(list_of_draws$yT)
  #create MCMC CI intervals for first and last 20 components of posterior yTtheta
  color_scheme_set("red")
  file_name2 = paste("First20comps_Month",i)
  jpeg(file_name2)
  first20_plt <- mcmc_intervals(colMeans(posterior_yTtheta[,,1:20]))
  print(first20_plt)
  dev.off()
  
  file_name3 = paste("Last20comps_Month",i)
  jpeg(file_name3)
  last20_plt <- mcmc_intervals(colMeans(posterior_yTtheta[,,(nrow-19):nrow]))
  print(last20_plt)
  dev.off()
  
  #generate MCMC histogram for first and last 20 components of posterior yTtheta
  # color_scheme_set("green")
  # first20_comps <- mcmc_hist(colMeans(posterior_yTtheta[,,1:20]), binwidth = 1.00)
  # last20_comps <- mcmc_hist(colMeans(posterior_yTtheta[,,(nrow-19):nrow]), binwidth = 1.00)
  # #save the graphs into different jpeg files
  # file_name4 = paste("Histogram_First20comps_Day",i)
  # file_name5 = paste("Histogram_Last20comps_Day",i)
  # jpeg(file_name4)
  # print(first20_comps)
  # dev.off()
  # jpeg(file_name5)
  # print(last20_comps)
  #dev.off()
  
  fit_summary <- summary(stanfit)
  file_name6 = paste("resultsummary_First&Last20components_ Month", i)
  jpeg(file_name6)
  print(fit_summary$summary)
  dev.off()
  
  color_scheme_set("mix-brightblue-gray")
  file_name = paste("Rhat histogram - Month", i)
  jpeg(file_name)
  rhats <- rhat(stanfit)
  rhat_plt <- mcmc_rhat_hist(rhats) + yaxis_text(hjust = 1) + ggtitle(paste("Rhat histogram - Month", i))
  print(rhat_plt)
  dev.off()
}
  
# require(sp)
# require(rgdal)
# require(maps)
# library(ptinpoly)

# lat_lon <- cbind(d$Long, d$Lat)
# 
# zone <- read_delim("/Users/tuanle/zone.csv", delim = ",", col_names = TRUE)
# for(i in 1:nrow(zone)){
#   zone$geo[i] = substr(zone$geo[i],10,135)
# }
# Numextract <- function(string){
#   unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)))
# }
# zone <- zone[complete.cases(zone),]
# polys <- vector("list", nrow(zone))
# for(i in 1:nrow(zone)){
#   polys[[i]] <- matrix(as.numeric(Numextract(zone$geo[i])), ncol=2, byrow=TRUE)
#   polys[[i]][,1] <- -polys[[i]][,1]
# }
# pnt.in.poly2 <- function(pnts, poly.pnts){
#   if (poly.pnts[1, 1] == poly.pnts[nrow(poly.pnts), 1] && poly.pnts[1, 2] == poly.pnts[nrow(poly.pnts), 2])
#     {poly.pnts = poly.pnts[-1, ]}
#   out = .Call("pip", pnts[, 1], pnts[, 2], nrow(pnts), poly.pnts[,1], poly.pnts[, 2], nrow(poly.pnts), PACKAGE = "SDMTools")
#   return(out)
# }
# lat_lon_list <- vector("list", 2*69+1)
# for(i in 1:(2*69)){
#   lat_lon_list[[i]] = lat_lon[(1+(i-1)*1e6/2):(i*1e6/2),]
# }
# lat_lon_list[[2*69+1]] <- lat_lon[69000001:nrow(lat_lon),]
# library(data.table)
# for(i in 1:(2*69+1)){
#   DT <- data.table(V1 = pnt.in.poly2(lat_lon_list[[i]], polys[[1]]))
#   for(j in 2:length(polys)){
#     DT[, (sprintf("V%d",j)):=pnt.in.poly2(lat_lon_list[[i]], polys[[j]])]
#   }
#   fwrite(DT, sprintf("results%02d.csv", i))
#   rm(DT)
# }


# combos <- sapply(stops$Lat,function(x) abs(x-d$Lat))
# combos2 <- sapply(stops$Long,function(x) abs(x-d$Long))
 
# d_zone2 <- dplyr::left_join(x = d, y = stops%>% select("Lat", "Long", X8), by = c(min(combos), min(combos2)))
d_zone$X8 <- as.numeric(d_zone$X8)
d_zone2 <- subset(d_zone, is.na(d_zone$X8))
d_zone2 <- d_zone2[order(d_zone2$Lat, d_zone2$Long),]

library(sp)
library(data.table)

setDT(d_zone2)

stop_points <- as.matrix(stops[, 3:2])
short <- unique(d_zone2, by = c("Long", "Lat"))
short_stop <- short[, ZONE := stops[which.min(spDists(x = stop_points, y = cbind(Long, Lat))),]$X8, by=.(Long, Lat)] 
short_stop <- short_stop[, -13]
short_stop <- dplyr::rename(short_stop, X8 = ZONE)
d_zone <- left_join(x = d%>% select("Stop_Name", X8), y = short_stop%>% select("Stop_Name", X8), by = c("Stop_Name"))
d_zone <- d_zone[order(d_zone$Stop_Name, d_zone$Lat, d_zone$Long),]
# for(i in 1:nrow(d_zone2)){
#   for(j in 1:nrow(stops)){
#     if(d_zone$Lat[i] >=   
#   }
# }
#d <- d[d$Date != "Date", ]

d1_ON = d%>% group_by(Date) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ON_Mth = d%>%  group_by(month) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ON_AVG = d%>% group_by(Date) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_ON_DOW = d%>% group_by(DOW) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ON_DOWAVG = d%>% group_by(DOW) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_ON_MthAVG = d%>%  group_by(month) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_ON <- d1_ON[order(d1_ON$Date, decreasing=FALSE),]
d1_ON$mth <- substr(d1_ON$Date,4,6)
d1_ON <- subset(d1_ON, select = c("Date", "n", "sum"))

d2_ON = d %>% group_by(Route) %>% summarize (n=n(), sum_on = sum(as.numeric(ON)), sum_off = sum(as.numeric(OFF)))
d2_new <- d2_ON[c('Route', 'sum_on')]
d2_ON_AVG = d %>% group_by(Route) %>% summarize (n=n(), mean = mean(as.numeric(ON)))
acf(d2_new, type = "covariance", plot = FALSE)

d3 = d %>% group_by(Date,Stop_Sequence) %>% summarize (n=n(), sum_on = sum(as.numeric(ON)), sum_off = sum(as.numeric(OFF)))
d3 <- as.data.frame(d3)
d3$Stop_Sequence <- as.numeric(d3$Stop_Sequence)

par(mfrow = c(2,1))
ON_DOW <- d1_ON_DOW$sum
ON_AVG <- d1_ON_AVG$mean
ON_Mth <- d1_ON_Mth$sum
ON_MthAVG <- d1_ON_MthAVG$mean
Dayofyear <- d1_ON$Date
Dayofweek <- factor(d1_ON_DOW$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
Month <- factor(d1_ON_Mth$month, levels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
ggplot(d1_ON_DOW,aes(y= ON_DOW, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total APC ON per day of a week")
ggplot(d1_ON_AVG,aes(y= ON_AVG, x = Dayofyear)) + geom_bar(stat="identity") + ggtitle("Average APC ON per each day")
ggplot(d1_ON_Mth,aes(y= ON_Mth, x = Month)) + geom_bar(stat="identity") + ggtitle("Total APC ON per each month")
ggplot(d1_ON_MthAVG,aes(y= ON_MthAVG, x = Month)) + geom_bar(stat="identity") + ggtitle("Average APC ON per each month")

ON <- d1_ON$sum
ggplot(d1_ON,aes(y= ON, x = Dayofyear)) + geom_bar(stat="identity") + ggtitle("Total APC ON per day of year 2015")

#read routing matrix into R from numpy
mat <- npyLoad("a.npy")
index_name <- read.csv("/Users/tuanle/routing-matrix-index.csv", sep = ',', stringsAsFactors = F, header = TRUE)


index_rowzero <- which(rowSums(mat)==0)

#eliminate all the stops that are not conencted to all the remaining stops
for(i in unique(index_name$geo_node_id)){
   for(j in unique(index_rowzero)){
     if(!is.na(index_name$geo_node_id[i]) && !is.na(index_rowzero[j]) && index_name$geo_node_id[i] == index_rowzero[j]){
        index_name$geo_node_id <- index_name$geo_node_id[-i]
       }
   }
}
c <- vector(mode = "numeric", length = length(unique(d3$Date)))
t_date <- unique(d3$Date) 
for(i in 1:length(unique(d3$Date))){
  c[i]<- nrow(filter(d3, d3$Date == t_date[i]))
}

count = as.vector(matrix(0, nrow = length(unique(d3$Stop_Sequence))))
for(i in 1:length(d3$Date))
  {for(j in 1:length(index_name$geo_node_id)){
    if(!is.na(d3$Stop_Sequence[i]) && !is.na(index_name$geo_node_id[j]) && d3$Stop_Sequence[i] == index_name$geo_node_id[j])
    {d3$Stop_Sequence[i] ==  index_name$X[j]}
    else {count[i] = count[i]+1;
          print(j);}
  count[i] = count[i]+1;
  }
  if(!is.na(count[i]) && count[i]==length(index_name$geo_node_id))
    {d3$Stop_Sequence[-i]}
}

#to match the ON/OFF count with the correct row in routing matrix
for (i in (1:length(d3$Stop_Sequence))){
  for (j in (1:length(index_name$geo_node_id))){
    if (as.numeric(d3$Stop_Sequence[i]) == as.numeric(index_name$geo_node_id[j])){
      d3[i,5] = index_name$X[j]
    }
  }
}
d4 <- data.frame(Stop_Sequence = double(),
                 sum_on = double(),
                 sum_off = double(),
                 n = double(),
                 stringsAsFactors=FALSE)

#to delete all the stops that do not belong to our APC dataset in 2015
for (i in (1:length(d3$Stop_Sequence))){
  for (j in (1:length(index_name$geo_node_id))){
    if (as.numeric(d3$Stop_Sequence[i]) == as.numeric(index_name$geo_node_id[j])){
      d4[i,] <- d3[i,]
    }
  }
}

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

temp = makeCacheMatrix(mat%*%cov_2%*%t(mat)+cov_1)
K_term <- cov_2%*%t(mat)%*%cacheSolve(temp)
# estSpikedCovariance(d1_ON$sum, gamma = NA,
#                     numOfSpikes = NA,
#                     method = c("KNTest", "median-fitting"),
#                     norm = c("Frobenius", "Operator", "Nuclear"),
#                     pivot = 1, statistical = NA,
#                     fit = NA)
# rand_numb <- sample(1:1000,1000,replace=T)
# covmat_2 <- genPositiveDefMat("eigen", dim = nrow(Mat), rangeVar = c(min(d1_ON$sum),max(d1_ON$sum)))

# sum(d1$Date == "01-APR")
# stops <- subset(d, d$Date == "01-APR")
# stops <- stops[order(stops$DOW, decreasing=FALSE),]
# name_stop <- stops$Stop_Name
# 
# length(unique(d$Stop_Name))
# 
# options(useFancyQuotes = FALSE)
# 
# for (i in unique(d$Date)){
#   stop <- subset(d, d$Date == i)
#   for (j in (nrow(stop$Stop_Name)-1)){
#      connected_stop[j] <- paste((stop$Stop_Name)[j], (stop$Stop_Name)[j+1], sep = "+")  
#   }
# }
#d = read.csv("/Users/tuanle/stops.txt", sep = ',', stringsAsFactors = F)

#stops = d %>% select(stop_id,stop_lat,stop_lon) %>% distinct()
stops = d %>% select(Lat,Long) %>% distinct()

sbbox <- make_bbox(lon = stops$Long, lat = stops$Lat, f = .05)
sq_map <- get_map(location = sbbox, source = "google")
ggmap(sq_map) + geom_point(data = stops, mapping = aes(x = Long, y = Lat), color = "red")

d %>% select(GeoNodeId,Lat,Long) %>% distinct() %>% count()
d %>% select(GeoNodeId) %>% distinct() %>% count()
d %>% select(Stop.Name, Lat,Long) %>% distinct() %>% count()
d %>% select(Stop.Name) %>% distinct() %>% count()
d %>% select(BusID) %>% distinct() %>% count()
d %>% select(Block) %>% distinct() %>% count()
d %>% select(Route) %>% distinct() %>% count()
d %>% select(Run) %>% distinct() %>% count()
d %>% select(TripTime) %>% distinct() %>% count()

stops = d %>% select(GeoNodeId,Lat,Long) %>% distinct()

sbbox <- make_bbox(lon = stops$Long, lat = stops$Lat, f = .1)
sq_map <- get_map(location = sbbox, source = "google")
ggmap(sq_map) + geom_point(data = stops, mapping = aes(x = Long, y = Lat), color = "red")

sum(d$ON)
sum(d$OFF)

colnames(d)
d$ON <- as.numeric(d$ON)
d$OFF <- as.numeric(d$OFF)
#d1 <- d[order(d$Date, decreasing=FALSE),]
by_dow  = d %>% group_by(DOW) %>% summarize(n = n(), mean = mean(ON))
by_dow$DOW <- factor(by_dow$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
#by_dow <- by_dow[with(by_dow, order(by_dow$mean, decreasing = TRUE)),]
by_dow2  = d %>% group_by(DOW) %>% summarize(n = n(), mean = mean(OFF))
by_dow2$DOW <- factor(by_dow2$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

by_dow_sum = d %>% group_by(DOW) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum$DOW <- factor(by_dow_sum$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

by_dow_sum2 = d %>% group_by(DOW) %>% summarize(n = n(), sum = sum(OFF))
by_dow_sum2$DOW <- factor(by_dow_sum2$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

#by_dow2 <- by_dow2[with(by_dow, order(by_dow2$mean, decreasing = TRUE)),]

# hist(by_dow$mean[2:8], main = "Histogram of average APC_ON by DOW", xlab = c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday"), ylab = "APC ON", border = "blue", col="green", xlim = c(0.24, 0.32), las=1, breaks=12)
# plot(density(by_dow$mean[2:7]))
# ggplot(data = d1,
#        aes(x= factor(d1$DOW, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), y = d1$ON)) +
#   stat_summary(fun.y= mean, geom = "bar")


par(mfrow = c(2,1))
Avg_APCON <- by_dow$mean
Dayofweek <- by_dow$DOW
ggplot(by_dow,aes(y= Avg_APCON, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Average APC ON per day of a week")

Avg_APCOff <- by_dow2$mean
Dayofweek <- by_dow2$DOW
ggplot(by_dow2, aes(y= Avg_APCOff, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Average APC Off per day of a week")

#d1$datetime <- paste(d1$Date, d1$Trip.Time)
# as.POSIXct(paste("01-OCT-16", d1$Date, sep = ""), format = "%d-%m-%y")
# d1$newdate <- strptime(as.character(d1$Date), "%d-%m-%Y")
# d1$newdate<- format(d1$newdate, "%Y-%m-%d")

# Sum <- aggregate(d1["ON", "OFF"], 
#                  list(hour=substr(d1$TripTime,1,2)),
#                  mean)

d2 <- d[order(d$Date, d$DOW, substr(d$Trip.Time,1,2)),]
by_dow3  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(ON))
#by_dow3 <- by_dow3[with(by_dow3, order(by_dow3$TripTime)),]
by_dow4  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(OFF))
#by_dow4 <- by_dow4[with(by_dow4, order(by_dow4$TripTime)),]
by_dow_sum3  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum4  = d2 %>% group_by(substr(d$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(OFF))

d3 <- d[order(d$Date),]
by_dow5 = d3 %>% group_by(Date) %>% summarize(n = n(), mean = mean(ON))
by_dow6  = d3 %>% group_by(Date) %>% summarize(n = n(), mean = mean(OFF))
by_dow_sum5 = d3 %>% group_by(Date) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum6 = d3 %>% group_by(Date) %>% summarize(n = n(), sum = sum(OFF))

par(mfrow = c(2,1))
barplot(by_dow3$mean, main = "Average APC ON by hour of the day", names.arg = sort(c(unique(substr(d$Trip.Time,1,2)))), decreasing = FALSE, ylab = "Mean APC ON", las= 2)
barplot(by_dow4$mean, main = "Average APC OFF by hour of the day",names.arg = sort(c(unique(substr(d$Trip.Time,1,2))), decreasing = FALSE), ylab = "Mean APC OFF", las=2)

par(mfrow = c(2,1))
barplot(by_dow5$mean, main = "Average APC ON by each day of the month", names.arg = c(1:31), ylab = "Mean APC ON", las = 3)
barplot(by_dow6$mean, main = "Average APC OFF by each day of the month", names.arg = c(1:31), ylab = "Mean APC OFF", las = 3)

Sum_APCON <- by_dow_sum3$sum
Hour <- unique(by_dow_sum3$`substr(d$Trip.Time, 1, 2)`)
Sum_APCOFF <- by_dow_sum4$sum
SUM_APCON_Month <-by_dow_sum5$sum
SUM_APCOFF_Month <- by_dow_sum6$sum

par(mfrow = c(2,1))
ggplot(by_dow_sum3,aes(y= Sum_APCON, x = Hour)) + geom_bar(stat="identity") + ggtitle("Total APC ON per hour of a day")
ggplot(by_dow_sum4,aes(y= Sum_APCOFF, x = Hour)) + geom_bar(stat="identity") + ggtitle("Total APC OFF per hour of a day")

par(mfrow = c(2,1))
barplot(by_dow_sum5$sum, main = "Total APC ON by each day of the month", names.arg = c(1:31), ylab = "Total APC ON", las = 3)
barplot(by_dow_sum6$sum, main = "Total APC OFF by each day of the month", names.arg = c(1:31), ylab = "Total APC OFF", las = 3)

d4 <- d[order(d$Date, decreasing=FALSE),]
by_dow7  = d4 %>% group_by(Route,month, Date) %>% summarize(n = n(), mean = mean(ON))
#by_dow3 <- by_dow3[with(by_dow3, order(by_dow3$Trip.Time)),]
by_dow8  = d4 %>% group_by(Route,month, Date) %>% summarize(n = n(), mean = mean(OFF))
b#by_dow4 <- by_dow4[with(by_dow4, order(by_dow4$Trip.Time)),]
by_dow_sum7 =  d4 %>% group_by(Route,month, Date) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum8 =  d4 %>% group_by(Route, month, Date) %>% summarize(n = n(), sum = sum(OFF))

by_dow9 = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(ON))
by_dow10  = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), mean = mean(OFF))

by_dow_sum9 = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum10  = d4 %>% group_by(Route, substr(d4$Trip.Time,1,2)) %>% summarize(n = n(), sum = sum(OFF))

by_dow11 = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), mean = mean(ON))
by_dow12  = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), mean = mean(OFF))

by_dow_sum11 = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), sum = sum(ON))
by_dow_sum12  = d4 %>% group_by(Route, d4$DOW) %>% summarize(n = n(), sum = sum(OFF))

par(mfrow = c(2,1))

newdata <- subset(by_dow7, by_dow7$Route == "208")
Day <- substr(newdata$Date,1,2)
APCON_R208 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCON_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC ON-Route208 per each day of October 2017")

newdata <- subset(by_dow8, by_dow8$Route == "208")
Day <- substr(newdata$Date,1,2)
APCoff_R208 <- newdata$mean
ggplot(newdata,aes(x = Day, y = APCoff_R208)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off-Route208 per each day of October 2017")

# #function to save output figures into file
# utils$pdf_w = 10.02
# utils$pdf_h = 7.27
# save_pdf <- function(path = './', name, lwidth = utils$pdf_w, lheight = utils$pdf_h)
# {
#   print(paste(path, paste(name,".pdf", sep="") ,sep=""))
#   dev.copy(pdf,paste(path, paste(name,".pdf", sep="") ,sep=""), width = lwidth, height = lheight);
#   dev.off();
# }

plot_list = list()
for (i in unique(by_dow_sum7$Route)) {
  newdata <- subset(by_dow_sum7, by_dow_sum7$Route == as.character(i))
  Day <- substr(newdata$Date,1,2)
  #Day <- unique(newdata$Date)
  APCON <- newdata$sum
  p = ggplot(newdata,aes(x = Day, y = APCON)) + geom_bar(stat="identity") + ggtitle(paste("Total APCON-Route", i, "per days of 2015"))
  plot_list[[i]] = p
  file_name = paste("Total APCON-Route", i, "per days of 2015.jpg")
  jpeg(file_name)
  print(plot_list[[i]])
  dev.off()
}  

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

#  fct <- function(i){
#   barplot(by_dow7$mean, main = "Average APC ON by each route per each day of the month", names.arg = c(i),ylab = "Mean APC ON")
#  }
# 
# sapply(unique(by_dow7$Route), fct)


# barplot(by_dow8$mean, main = "Average APC OFF by each route per each day of the month", names.arg = ylab = "Mean APC OFF")

par(mfrow = c(2,1))

# APCon <- by_dow9$mean
# DoM3 <- by_dow9$`substr(d4$Trip.Time, 1, 2)`
# RouteNumber <- by_dow9$Route
# 
# ggplot(by_dow9,aes(x = DoM3, y = APCon, fill= RouteNumber),las=3)  + geom_bar(stat="identity", position = "dodge") + ggtitle("Average APC Off per route per each day of October 2017") 

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


# ggplot(by_dow10,aes(x = by_dow10$Route, y = by_dow10$mean, fill=row))  + geom_bar(stat="identity",aes(fill= by_dow10$'substr(d4$TripTime, 1, 2)')) + xlab("\nBus Route") + ylab("Average APC-OFF per hour") 
# 
# par(mfrow = c(2,1))
# 
# ggplot(by_dow11,aes(x = by_dow11$Route, y = by_dow11$mean, fill=row))  + geom_bar(stat="identity",aes(fill= by_dow11$'d4$DOW')) + xlab("\nBus Route") + ylab("Average APC-ON per each day of a week")
# 
# 
# 
# ggplot(by_dow12,aes(x = by_dow12$Route, y = by_dow12$mean, fill=row))  + geom_bar(stat="identity",aes(fill= by_dow12$'d4$DOW')) + xlab("\nBus Route") + ylab("Average APC-OFF per each day of a week")



Sum_APCON <- by_dow_sum$sum
Dayofweek <- by_dow_sum$DOW
Sum_APCOFF <- by_dow_sum2$sum
Dayofweek <- by_dow_sum2$DOW
par(mfrow = c(2,1))
ggplot(by_dow,aes(y= Sum_APCON, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total APC ON per day of a week")
ggplot(by_dow,aes(y= Sum_APCOFF, x = Dayofweek)) + geom_bar(stat="identity") + ggtitle("Total APC OFF per day of a week")


df1 <- data.frame(d$Lat, d$Long, d$ON, d$OFF)
# df_1 <- transform(df1, bin = cut(d$Long, 10))
# df2 <- transform(df1, bin = cut(d$Lat, 20))
# df3 <- transform(df1, bin = cut(d$Lat, 50))
# df4 <- transform(df1, bin = cut(d$Lat, 100))
# df5 <- transform(df1, bin = cut(d$Lat, 200))
# df6 <- transform(df1, bin = cut(d$Lat, 400))
# df7 <- transform(df1, bin = cut(d$Lat, 500))

df <- df1 %>% group_by(cut(d$Long,10), cut(d$Lat,20)) %>% summarize(n = n(), sum = sum(d.ON)) #64 rows
df2 <- df1 %>% group_by(cut(d$Long,10), cut(d$Lat,20)) %>% summarize(n = n(), sum = sum(d.OFF)) #64 rows
df3 <- df1 %>% group_by(cut(d$Lat,50), cut(d$Long,50)) %>% summarize(n = n(), sum = sum(d.ON)) #496 rows
df4 <- df1 %>% group_by(cut(d$Lat,75), cut(d$Long,75)) %>% summarize(n = n(), sum = sum(d.OFF)) #666 rows

df5 <- df1 %>% group_by(cut(d$Long,100), cut(d$Lat,100)) %>% summarize(n = n(), sum = sum(d.ON)) #900 rows
df6 <- df1 %>% group_by(cut(d$Long,50), cut(d$Lat,100)) %>% summarize(n = n(), sum = sum(d.OFF)) #638 rows
df7 <- df1 %>% group_by(cut(d$Lat,200), cut(d$Long,200)) %>% summarize(n = n(), sum = sum(d.ON)) #1685 rows
df8 <- df1 %>% group_by(cut(d$Lat,50), cut(d$Long,100)) %>% summarize(n = n(), sum = sum(d.OFF)) #646 rows

df9 <- df1 %>% group_by(cut(d$Lat,500), cut(d$Long,500)) %>% summarize(n = n(), sum = sum(d.ON)) #2850 rows
df10 <-df1 %>% group_by(cut(d$Lat,500), cut(d$Long,500)) %>% summarize(n = n(), sum = sum(d.OFF)) #2850 rows

install.packages("ggmap", type = "source")
install.packages("gridExtra")
library(ggmap)
library(gridExtra)
library(scales)

qmap('chicago', zoom = 13)
dataset2 <- subset(d, 0 < d$ON | 0 < d$OFF)
keep <- c("Date", "DOW", "Block", "Route", "Direction","Trip.ID","Stop.Name","ON","OFF","Lat","Long")
dataset2 <- subset(dataset2 , select = keep)
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")

require('ggmap')
map.in <- get_map(location = c(min(d_zone$Long),
                               min(d_zone$Lat),
                               max(d_zone$Long),
                               max(d_zone$Lat)),
                  source = "google")
ggmap(map.in) + geom_point(data = stops, mapping = aes(x = lon, y = lat), color = "red")

map.in2 <- get_map(location = c(min(dataset2$Long),
                                min(dataset2$Lat),
                                max(dataset2$Long),
                                max(dataset2$Lat)),
                   source = "google")
theme_set(theme_bw(base_size = 8))
colormap <- c("Violet","Blue","Green","Yellow","Red","White")
d$DOW <- factor(d$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))
dataset2$DOW <- factor(dataset2$DOW, levels = c("Monday   ","Tuesday  ", "Wednesday","Thursday ","Friday   ", "Saturday ", "Sunday   "))

d1_ONzone = d_zone%>% group_by(DOW, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_ONAVG = d_zone %>% group_by(DOW, Long, Lat) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_OFFzone = d_zone%>% group_by(DOW, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(OFF)))
d1_OFFAVG = d_zone %>% group_by(DOW, Long, Lat) %>% summarize(n=n(), mean = mean(as.numeric(OFF)))
d_zone$month <- substr(d_zone$Date,4,6)
d1_ONzone_month = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(ON)))
d1_OFFzone_month = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), sum = sum(as.numeric(OFF)))
d1_ONzone_monthmean = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), mean = mean(as.numeric(ON)))
d1_OFFzone_monthmean = d_zone%>% group_by(month, Long, Lat) %>% summarize(n=n(), mean = mean(as.numeric(OFF)))

map.in <- get_map(location = c(min(d1_ONzone_monthmean$Long),
                               min(d1_ONzone_monthmean$Lat),
                               max(d1_ONzone_monthmean$Long),
                               max(d1_ONzone_monthmean$Lat)),
                  source = "google")
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

# Chicago <- get_map(location = c(long = -87.69, lat = 43.2), maptype = "roadmap", zoom = 7, source = "google")
# 
# ChicagoMap <- ggmap(Chicago, base_layer = ggplot(aes(x = Long, y = Lat, fill = ON), data = dataset2))
# 
# ChicagoMap + geom_polygon(aes(x = Long, y = Lat, fill = ON), data = dataset2, colour = "black", size = 1) +  scale_fill_gradient(low = "yellow", high = "red") + facet_wrap(~ DOW) + theme_nothing(legend = TRUE) + labs(title = "Heat Map for APC ON per each day of a week", fill = "")

# overlay <- stat_density2d(aes(color = ON, fill = ..level.., alpha = ..level..), bins = 15, geom = "polygon", data = dataset2)
# 
# ggmap(Chicago) +geom_point(data = dataset2,aes(x=Lat,y= Long,colour=ON)) + scale_fill_gradient(low = "blue", high="red") + facet_wrap(~ DOW)


ChicagoMap + overlay2 + scale_fill_gradient(low = "yellow", high = "red") + facet_wrap(~ DOW)


