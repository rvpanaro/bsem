data{
  int<lower=1> K;
  int<lower=1> Nv;
  int<lower=1> Ne;
  int<lower=1> Nob;
  int<lower=1> idob[Nob,2];
  matrix[Nv,Ne] X;
  matrix<lower=0> [Nv,K] v;
  real<lower=0> a;
  real<lower=0> b;

}

parameters{
  matrix[Nv,K] alpha;
  matrix[K,Ne] lambda;
  vector<lower=0> [Nv] sigma2;
}

model{
  for(i in 1:Nob){
    X[idob[i,1],idob[i,2]] ~ normal(alpha[idob[i,1],] * lambda[,idob[i,2]], sqrt(sigma2[idob[i,1]]));
  }

  // Factor analysis model

  // Loadings prior
   for(k in 1:K){
      alpha[,k] ~ normal(0, sqrt(v[,k]));
  }

  // Scores prior
  to_vector(lambda) ~ normal(0, 1);

  // error variance prior
  sigma2 ~ inv_gamma(a, b);

}


// empty line to avoid messages
