//data block
data {
  int<lower=1> ncol; //specify number of columns of routing matrix A, historical demand vector yH and ON                         count vector x
  int<lower=1> nrow; //specify number of rows of routing matrix A
  vector[ncol] yH; //historical demand vector yH
  int x[nrow]; //x ~ Poisson, so it's discrete count data!
  matrix[nrow, ncol] A;
  //vector[nrow] sigma_x;
  //vector[ncol] sigma_y;
  vector[nrow] epsilon;
  vector[nrow] epsilon2;
}

//parameter block. estimated demand variable yT
parameters {
   vector<lower = 0>[ncol] yT;
}

//model block
model {
  //prior
  //yT ~ normal(yH, sigma_y); //Normal distribution
  yT ~ normal(yH, sqrt(yH + epsilon2)); //Normal distribution used to approximate Poisson distribution                                             with rate yH
  
  //likelihood ~ Poisson(rate = x') where epsilon stands for noise in our estimation
  x ~ poisson(A*yT + epsilon);
}



