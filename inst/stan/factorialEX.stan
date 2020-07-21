data{
  int<lower=1> K;
  int<lower=1> Nv;
  int<lower=1> Ne;
  int<lower=1> Nob;
  int<lower=1> idob[Nob,2];
  matrix[Nv,Ne] X;

  int<lower=1> Nex;                   // n of exogenous variables
  int<lower=1> ngamma [Nex];          // number of gammas in each regression
  int<lower=1> idexi [sum(ngamma)];   // factors explaining exogenous identifier
  int<lower=1> idex[Nex];             // (dependent) exogenous identifier
  int<lower=1> idlambex [sum(ngamma)]; // scores identifier

  matrix<lower=0> [Nv-Nex,K] v;
  int<lower=1> asc [Nob];

  real<lower=0> dsigma2 [Nv-Nex];
  real a [Nv-Nex];
  real<lower=0> b [Nv-Nex];
  real<lower=0> dtau2 [Nex];
  real at [Nex];
  real<lower=0> bt [Nex];

  real<lower=0> dgamma [sum(ngamma)];
  real<lower=0> mg [sum(ngamma)];
  real<lower=0> sg [sum(ngamma)];
  real<lower=0> dgamma0 [Nex];
  real<lower=0> mg0 [Nex];
  real<lower=0> sg0 [Nex];
}

parameters{
  matrix[Nv-Nex,K] alpha;
  matrix[K,Ne] lambda;
  vector<lower=0> [Nv-Nex] sigma2;

  row_vector[Nex] gamma0;
  row_vector[sum(ngamma)] gamma;
  row_vector<lower=0> [Nex] tau2;
}

model{
  for(i in 1:Nob){
    X[idob[i,1],idob[i,2]] ~ normal(alpha[asc[i],] * lambda[,idob[i,2]], sqrt(sigma2[asc[i]]));
  }

   // exogenous regression
   X[idex[1],] ~ normal(gamma0[1] + gamma[1:ngamma[1]]*lambda[idlambex[1:ngamma[1]],], sqrt(tau2[1]));

  if(Nex>1){
    for(i in 2:Nex){
     X[idex[i],] ~ normal(gamma0[i] + gamma[(sum(ngamma[1:(i-1)])+1):sum(ngamma[1:i])]*lambda[idlambex[(sum(ngamma[1:(i-1)])+1):sum(ngamma[1:i])],], sqrt(tau2[i]));
    }
  }

  // Factor analysis model

  // Loadings prior
   for(k in 1:K){
      alpha[,k] ~ normal(0, sqrt(v[,k]));
  }

  // Scores prior
  to_vector(lambda) ~ normal(0, 1);

  for(i in 1:(Nv-Nex)){
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

  for(i in 1:Nex){
    // tau2 prior
    if(dtau2[i] == 0){
      tau2[i] ~ gamma(at[i], bt[i]);
    }
    else if (dtau2[i] == 1){
      tau2[i] ~ inv_gamma(at[i], bt[i]);
    }
    else{
      tau2[i] ~ lognormal(at[i], bt[i]);
    }
  }

  for(i in 1:Nex){
    if(dgamma0[i] == 0){
      gamma0[i] ~ normal(mg0[i], sqrt(sg0[i]));
    }
    else{
      gamma0[i] ~ cauchy(mg0[i], sqrt(sg0[i]));
    }
  }

  for(i in 1:sum(ngamma)){
    if(dgamma[i] == 0){
      gamma[i] ~ normal(mg[i], sqrt(sg[i]));
    }
    else{
      gamma[i] ~ cauchy(mg[i], sqrt(sg[i]));
    }
  }

}


// empty line to avoid messages
