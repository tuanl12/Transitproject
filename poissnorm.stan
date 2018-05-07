
data {
  int<lower=1> ncol;
  int<lower=1> nrow;
  vector[ncol] yH;
  //vector[nrow] x;
  int x[nrow];
  matrix[nrow, ncol] A;
  //vector[nrow] sigma_x;
  //vector[ncol] sigma_y;
  //matrix[nrow,ncol] sigma_x;
  //matrix[nrow,ncol] sigma_y;
  vector[nrow] epsilon;
  vector[nrow] epsilon2;
}

parameters {
   vector<lower = 0>[ncol] yT;
}

model {
  //prior
  //yT ~ normal(yH, sigma_y);
  yT ~ normal(yH, sqrt(yH + epsilon2));
  //yT ~ multi_normal(yH, sigma_y);
  //likelihood
  x ~ poisson(A*yT + epsilon);

  //for(n in 1:nrow){
  //  x[n] ~ poisson(A[n,]*yT + epsilon[n]);
  //}
}



