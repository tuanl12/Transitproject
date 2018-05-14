//data block.
data { 
  int<lower=1> ncol; //specify number of columns of routing matrix A, historical demand vector yH and ON                         count vector x
  int<lower=1> nrow; //specify number of rows of matrix A
  vector[ncol] yH; //historical demand yH with size = ncol*1
  vector[nrow] x; 
  matrix[nrow, ncol] A; //routing matrix A
  //noises data in prior and likelihood
  vector[nrow] sigma_x; 
  vector[ncol] sigma_y;
  //expected difference between A*yT and x
  vector[nrow] epsilon;
  //vector[nrow] epsilon2;
}

//parameter block - estimated demand variable yT
parameters {
   vector<lower = 0>[ncol] yT;
}

#specify the model
model {
  yT ~ normal(yH, sigma_y); //prior ~ Normal distribution with mean = yH, variance = sigma_y
 //prior ~ Poisson(rate = yH), which is approximated by a Normal distribution with mean = yH and variance  //= sqrt(yH + epsilon2)
  //yT ~ normal(yH, sqrt(yH + epsilon2)); 

   x ~ normal(A*yT + epsilon, sigma_x); //likelihood ~ Normal distribution with mean = A*yT + epsilon and                                          variance  = sigma_x
}



