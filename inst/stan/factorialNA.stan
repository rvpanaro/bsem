data{
  int<lower=1> K;
  int<lower=1> Nv;
  int<lower=1> Ne;
  int<lower=1> Nob;
  int<lower=1> Nna;
  matrix[Nv,Ne] X;
  int<lower=1> idob[Nob,2];
  int<lower=1> idna[Nna,2];
  matrix<lower=0> [Nv,K] v;

  real<lower=0> dsigma2 [Nv];
  real a [Nv];
  real<lower=0> b [Nv];
}

parameters{
  matrix[Nv,K] alpha;
  matrix[K,Ne] lambda;
  vector<lower=0> [Nv] sigma2;
  vector[Nna] Xna;
}

model{
  for(i in 1:Nob){
    X[idob[i,1],idob[i,2]] ~ normal(alpha[idob[i,1],] * lambda[,idob[i,2]], sqrt(sigma2[idob[i,1]]));
  }

  // Loadings prior
   for(k in 1:K){
      alpha[,k] ~ normal(0, sqrt(v[,k]));
    }

  // scores prior
  to_vector(lambda) ~ normal(0, 1);

  // missing data prior
  for(i in 1:Nna){
    Xna[i] ~ normal(alpha[idna[i,1],] * lambda[,idna[i,2]], sqrt(sigma2[idna[i,1]]));
  }

  // error variance prior
  for(i in 1:Nv){
    // sigma2 prior
    if(dsigma2[i] == 0){
      sigma2[i] ~ gamma(a[i], b[i]);
    }
    else if (dsigma2[i] == 1){
      sigma2[i] ~ inv_gamma(a[i], b[i]);
    }
    else{
      sigma2[i] ~ lognormal(a[i], b[i]);
    }
  }
}
// empty last line to avoid messages

