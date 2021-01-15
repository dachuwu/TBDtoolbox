data {
 int<lower=0> n_obs;
 int<lower=0> isDat[n_obs];
 int<lower=0> n_x;
 int<lower=0> n_h;
 int<lower=0> k_vect[n_h];

 matrix[n_obs, n_x] X_fix;
 int X_hier[n_obs, n_h];

 real Y[n_obs];
 real<lower=0> pop[n_obs];
 real<lower=0> sdv[n_obs];
}

transformed data{

  int X_hier_pad[n_obs, n_h] = X_hier;
  if(n_h > 1){
    for(h in 2:n_h)
      for(i in 1:n_obs) X_hier_pad[i,h] = X_hier_pad[i,h] + sum(k_vect[1:(h-1)]);
  }

}

parameters {
  vector[n_x] b_x;
  //real a_0;
  real<lower=0> sig;
  real a_h[sum(k_vect)];
  real<lower=0> kap_a_h[n_h];
}

transformed parameters{
  real eta[n_obs];
  vector[n_obs] eta_x;

  eta_x =  X_fix * b_x;
  for(i in 1:n_obs){
    eta[i] = sum(a_h[X_hier_pad[i,]]) + eta_x[i];
  }
}

model {
  b_x ~ normal(0, 10);
  //a_0 ~ normal(0, 1);
  a_h[1:k_vect[1]] ~ normal(0, kap_a_h[1]);
  if(n_h > 1){
    for(h in 2:n_h)
      a_h[(sum(k_vect[1:(h-1)]) + 1) : sum(k_vect[1:h])] ~ normal(0, kap_a_h[h]);
  }
  kap_a_h ~ exponential(0.01);
  sig ~ exponential(0.01);
  for(i in 1:n_obs){
    if(isDat[i]==1) target += normal_lpdf(Y[i] | eta[i], sqrt(sig^2/pop[i]));
    if((sdv[i]>0) && pop[i]>1) target += chi_square_lpdf((sdv[i]^2)*(pop[i]-1)/(sig^2) | pop[i]-1 );
  }
}

