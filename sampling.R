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

#vector to store the MAPE and MSE error for each of 365 days. 
made_err <- rep(0,365)
mse_err <- rep(0,365)
#data preparation for inputting into Stan. Running for loops over 365 days
for(i in 1:365) {
  #denote the dimensions for true demand vector yT (denoted as d' in the paper) and ON counts data x
  nrow = nrow(rte_m[[i]]);
  ncol = ncol(rte_m[[i]]);
  #assign routing matrix to a new variable A (same notation as in the paper)
  A <- as.matrix(rte_m[[i]]);
  #variance vectors sigma_x^2, sigma_y^2 (both drawn from U(0,10)) for prior and likelihood (denoted the same as in the paper).
  #sigma_x <- as.vector(sample.int(10, nrow(kf_vect[[i]]), replace=TRUE))
  sigma_y <- as.vector(sample.int(10, nrow(kf_vect[[i]]), replace=TRUE))
  #assign historical demand, which is the solution given by least-squares, into variable "yh" (denoted as "dh" in the paper) 
  yH <- as.vector(dh_vect[[i]]$X);
  #create a simulated true demand "yT" (denoted as d' in the paper) with formula "yH + epsilon" where epsilon ~ U(0,10)
  yT <- yH + as.vector(eps_vect[[i]]); 
  #difference between A*yT and x. This represents the term 'epsilon_1 ~ U(0,20)' denoted in the paper
  epsilon <- sample.int(20, nrow(kf_vect[[i]]), replace=TRUE)
  #noise (denoted as 'k in the paper) added to the variance of the normal distribution approximation for Poisson.
  #epsilon2 <- runif(nrow(kf_vect[[i]]),0.02, 0.9)
  #simulate the currently observed ON counts data "x" (denoted as d' in the paper) by the formula: A*yT + epsilon 
  #(epsilon accounts for noises in our currently observed counts data 'x')
  x <- round(as.vector(as.matrix(rte_m[[i]])%*%yT) + epsilon)
  #denote number of iterations for each time one MCMC chain is run
  iterations = 250;
  
  #input our Stan model file "stamodeling.stan" (for the two cases when the likelihood ~ Normal(A*d+epsilon_1, sigma_1), as denoted in the paper).
  #input our Stan model file "poissnorm.stan" (for the two cases when the likelihood ~ Poisson(A*d+epsilon_1))
  stanmodel1 <- stan_model(file = "poissnorm.stan",
                           model_name = "stanmodel1");
  
  #Hamiltonian Monte Carlo to generate posterior distribution. Iter = # of iterations per one MCMC chain, chains = number of chains performed by MCMC
  stanfit <- sampling(stanmodel1, cores = parallel::detectCores(), data = list(ncol = ncol,nrow = nrow,
                                                                               yH = yH, 
                                                                               x=x, epsilon = epsilon,
                                                                               A = A, sigma_y = sigma_y,
                                                                               sigma_x = sigma_x)
                      ,iter=iterations, chains = 4, control = list(max_treedepth=14));
  print(i)
  #extract the predicted demand labeld as "posterior_yTtheta" (denoted as d) and create the comparison plots.
  posterior_yTtheta <- as.array(stanfit,  pars = "yT")
  #store the graph on the .jpeg file titled "Comparison_KFnnls_Day..."
  file_name10 <- paste("Comparison_KFnnls_Day", i, ".jpeg")
  jpeg(file_name10)
  #create comparison plots between estimated demand (colMeans(posterior_yTtheta[,3,])), Kalman-Filter (only for Normal - Normal case), 
  #and simulated true demand yT 
  plot(colMeans(posterior_yTtheta[,3,]),col=1, type = 'l', ylab="d", xlab = "Zone index")
  lines(yT,col=2, type = 'l', ylab = "d")
  #only use when running for the Normal - Normal case. 
  #lines(mean_kf[[i]], col=3, type = 'l', ylab = "d")
  #choose the first one for the 3 cases (Normal - Poisson/Poisson - Poisson/Poisson - Normal) and plot the estimated demand d versus true demand d'
  legend(x = "topleft",legend = c("recovered", "true"), col=1:2, lty=c(1,1))
  #this one is only for the first case (Normal - Normal) since there is a Kalman-Filter solution in that scenario.
  #legend(x = "topleft",legend = c("recovered", "true", "Kalman-Filter"), col=1:3, lty=c(1,1,1))
  
  #compute MAPE using the formula: 1/N*sum(absolute((y_predicted - y_true)/y_true))
  made_err[i] <- (mean(abs((colMeans(posterior_yTtheta[,3,]) - yT)/yT)))*100
  #compute MSE using the formula: 1/N*sum(y-y_true)^2
  mse_err[i] <- mean((colMeans(posterior_yTtheta[,3,]) - yT[1])^2)
  #generate all the histograms for each component of the estimated demand vector d for each day.   
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
