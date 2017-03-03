data {
  int<lower=0> N;
  real y[N];
  //real x[N];
  real<lower = 0> se[N];
}

parameters {
  real mu;
  real theta[N];
  real <lower = 0> tau;
}

model {
  y ~ normal(theta,se);
  theta ~ normal(mu,tau);
}
