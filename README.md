# Bayesian Forecast for Transit Demand 

This is the place to store the codes for performing exploratory data analysis tasks on APC and Ventra dataset (file `APC_EDA.R` and `Ventra_EDA.R`, respectively). I perform the MCMC simulation on Amazon Web Server using the modeling file in Stan (`stamodeling.stan` for Normal prior and `poissnorm.stan` file for Poisson prior) and the execution file is in R (`sampling.R`). 

Instruction to conduct MCMC simulation: 
1. Choose the model file `stamodeling.stan` for the case when likelihood ~ Normal, or `poissnorm.stan` for the case when likelihood ~ Poisson
2. Load the "sampling.R" file, load all the library packages (especially library(Rstan)). Then run the two lines allow `rstan_options()` and `options(mc.cores = parallel::detectCores())` to allow parallel processing for speeding up the MCMC simulation process (please do this! Otherwise, MCMC simulation for this particular problem might crash your R session.)
3. For each of the 4 cases, if you fix the likelihood, then depending on whether it is Normal or Poisson, go straight to `stamodeling.stan` or `poissnorm.stan` file, and comment out the line displaying the current prior. Uncomment the line right below, and go to data block to uncomment out the data input essential to that prior. Comment out the remaining data of the previous prior.
4. Run all the codes in `sampling.R` file.
