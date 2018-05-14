#should load all of this library!
library(rstan) #need to load this!
library(MASS)
library(bayesplot)
library(bridgesampling)
library(MCMCpack)
library(bayesplot)
library(ggplot2)
library(Hmisc)
library(coda)

#setting to run Stan with multi-cores option. Please run it before you do MCMC to speed up the sampling!
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#vector to store the MAPE
made_err <- rep(0,365)
#data preparation for inputting into Stan
for(i in 350:365) {
  nrow = nrow(rte_m[[i]]);
  ncol = ncol(rte_m[[i]]);
  #routing matrix A
  A <- as.matrix(rte_m[[i]]);
  #variance vectors sigma_x, sigma_y for prior and likelihood
  sigma_x <- as.vector(sample.int(20, nrow(kf_vect[[i]]), replace=TRUE))
  sigma_y <- as.vector(sample.int(10, nrow(kf_vect[[i]]), replace=TRUE))
  #historical demand
  yH <- as.vector(dh_vect[[i]]$X);
  #create a simulated true demand with formula yH + epsilon where epsilon ~ U(0,10)
  yT <- yH + as.vector(eps_vect[[i]]); 
  #difference between A*yT and x
  epsilon <- sample.int(20, nrow(kf_vect[[i]]), replace=TRUE)
  #noise added to the variance of the normal distribution approximation for Poisson.
  epsilon2 <- runif(nrow(kf_vect[[i]]),0.02, 0.9)
  #obtain the counts data
  x <- round(as.vector(as.matrix(rte_m[[i]])%*%yT) + epsilon)
  iterations = 250;
  
  #input our Stan model file "stamodeling.stan" (for the two cases when likelihood ~ Normal) 
  #into the object called "stanmodel1" 
  stanmodel1 <- stan_model(file = "stamodeling.stan",
                           model_name = "stanmodel1");
  
  #Hamiltonian Monte Carlo to generate posterior distribution. Iter = # of iterations per one MCMC chain, chains = number of chains performed by MCMC
  stanfit <- sampling(stanmodel1, cores = parallel::detectCores(), data = list(ncol = ncol,nrow = nrow,
                                                                               yH = yH, 
                                                                               x=x, epsilon = epsilon,
                                                                               A = A, sigma_y = sigma_y,
                                                                               sigma_x = sigma_x)
                      ,iter=iterations, chains = 4, control = list(max_treedepth=14));
  print(i)
  #extract the result with parameters "yT" and create the comparison plots.
  posterior_yTtheta <- as.array(stanfit,  pars = "yT")
  #store the graph on the .jpeg file titled "Comparison_KFnnls_Day..."
  file_name10 <- paste("Comparison_KFnnls_Day", i, ".jpeg")
  jpeg(file_name10)
  #create comparison plots between estimated demand(colMeans(posterior_yTtheta[,3,])), Kalman-Filter (only for Normal - Normal case), 
  #and simulated true demand yT 
  plot(colMeans(posterior_yTtheta[,3,]),col=1, type = 'l', ylab="d", xlab = "Zone index")
  lines(yT,col=2, type = 'l', ylab = "d")
  lines(mean_kf[[i]], col=3, type = 'l', ylab = "d")
  #choose one of these two depending on specific prior-likelihood
  #legend(x = "topleft",legend = c("recovered", "true"), col=1:2, lty=c(1,1))
  legend(x = "topleft",legend = c("recovered", "true", "Kalman-Filter"), col=1:3, lty=c(1,1,1))
  
  #compute MAPE
  made_err[i] <- (mean(abs((colMeans(posterior_yTtheta[,3,]) - yT)/yT)))*100

  #generate all the histograms for each component of the estimated demand vector d   
  for(j in 1:dim(posterior_yTtheta)[3]){
    file_name11 = vector("list", nrow(rte_m[[i]]))
    file_name11[[j]] <- paste("Histogram of demand of zone",j, "per day", i, ".jpeg")
    jpeg(file_name11[[j]])
    hist(posterior_yTtheta[,,j], main = paste("Distribution of demand of zone", j, "per day",i))
    dev.off()
  }
  #obtain the descriptive statistic of MCMC object "stanfit". Print out the statistics.
  fit_summary <- summary(stanfit)
  print(fit_summary$summary)
  
  #create the histogram of R_hat to check for convergence of MCMC's result.
  color_scheme_set("mix-brightblue-gray")
  file_name = paste("Rhatnnls_histogram_Day", i)
  jpeg(file_name)
  rhats <- rhat(stanfit)
  rhat_plt <- mcmc_rhat_hist(rhats) + yaxis_text(hjust = 1) + ggtitle(paste("Rhatnnls_histogram_Day", i))
  print(rhat_plt)
  dev.off()
}

