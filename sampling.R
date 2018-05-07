library(rstan)
library(MASS)
#library(truncnorm)
library(bayesplot)
library(bridgesampling)
library(MCMCpack)
library(bayesplot)
library(ggplot2)
library(coda)
library(clusterGeneration)
require(MASS)

library(doMC)
registerDoMC(cores=8)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#data preparation for inputting into Stan
for(i in 350:356) {
  nrow = nrow(rte_m[[i]]);
  ncol = ncol(rte_m[[i]]);
  A <- as.matrix(rte_m[[i]]);
  #sigma_x <- as.vector(sample.int(10, nrow(kf_vect[[i]]), replace=TRUE))
  #sigma_y <- as.vector(sample.int(8, nrow(kf_vect[[i]]), replace=TRUE))
  # sigma_x <- diag(1, nrow)
  # sigma_y <- diag(1, nrow)
  yH <- as.vector(dh_vect[[i]]$X);
  yT <- yH + as.vector(eps_vect[[i]]); 
  epsilon <- sample.int(20, nrow(kf_vect[[i]]), replace=TRUE)
  epsilon2 <- runif(nrow(kf_vect[[i]]),0.02, 0.9)
  x <- round(as.vector(as.matrix(rte_m[[i]])%*%yT) + epsilon)
  iterations = 100;
  #input data into a list called stan_data
  # stan_data = list(nrow = nrow, ncol = ncol,
  #                  yH = yH, 
  #                  x = x, epsilon = epsilon,
  #                  A = A, sigma_x = sigma_x, sigma_y = sigma_y);
  #input it into our Stan model file "stamodeling.stan"
  stanmodel1 <- stan_model(file = "poissnorm.stan",
                           model_name = "stanmodel1");
  
  #NUTS (No U-Turn) sampler to generate posterior distribution
  stanfit <- sampling(stanmodel1, cores = parallel::detectCores(), data = list(ncol = ncol,nrow = nrow,
                                                                               yH = yH, 
                                                                               x=x, epsilon = epsilon,
                                                                               A = A,
                                                                               epsilon2 = epsilon2)
                      ,iter=iterations, chains = 3, control = list(max_treedepth=14));
  print(i)
  #extract the result and plot graph
  posterior_yTtheta <- as.array(stanfit,  pars = "yT")
  file_name10 <- paste("Comparison_KFnnls_Day", i, ".jpeg")
  jpeg(file_name10)
  # #plot(yH, col=1, type='l', ylab="d", xlab = "Zone index", ylim=c(0,max(yT))) 
  plot(colMeans(posterior_yTtheta[,3,]),col=1, type = 'l', ylab="d", xlab = "Zone index")
  lines(yT,col=2, type = 'l', ylab = "d") 
  # #lines(mean_kf[[i]], col=4, type = 'l', ylab = "d")
  legend(x = "topleft",legend = c("recovered", "true"), col=1:2, lty=c(1,1))
  # #legend(x = "topleft",legend = c("recovered", "true", "Kalman-Filter"), col=1:3, lty=c(1,1,1))
  dev.off()
  
  # for(j in 1:dim(posterior_yTtheta)[3]){
  #   file_name11 = vector("list", nrow(rte_m[[i]]))
  #   file_name11[[j]] <- paste("Histogram of demand of zone",j, "per day", i, ".jpeg")
  #   jpeg(file_name11[[j]])
  #   hist(posterior_yTtheta[,,j], main = paste("Distribution of demand of zone", j, "per day",i))
  #   dev.off()
  # }
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
  
  # fit_summary <- summary(stanfit)
  # file_name6 = paste("resultsummary_KFnnls_ Day", i)
  # jpeg(file_name6)
  # print(fit_summary$summary)
  # dev.off()
  # 
  # color_scheme_set("mix-brightblue-gray")
  file_name = paste("Rhatnnls_histogram_Day", i)
  jpeg(file_name)
  rhats <- rhat(stanfit)
  rhat_plt <- mcmc_rhat_hist(rhats) + yaxis_text(hjust = 1) + ggtitle(paste("Rhatnnls_histogram_Day", i))
  print(rhat_plt)
  dev.off()
}

