
data{
  int<lower=1> K;
  int<lower=1> Nv;
  int<lower=1> Ne;

  int<lower=1> Ny;                  // n of dependent scores
  int<lower=1> idyi[K-Ny];          // independent scores identifier
  int<lower=1> idy[Ny];             // dependent scores identifier
  int<lower=1> nbeta [Ny];          // number of betas in each regression
  int<lower=1> idlamb [sum(nbeta)]; // scores identifier

  int<lower=1> Nob;
  int<lower=1> Nna;
  matrix[Nv,Ne] X;
  int<lower=1> idob[Nob,2];
  int<lower=1> idna[Nna,2];

  int<lower=1> Nex;                   // n of exogenous variables
  int<lower=1> ngamma [Nex];          // number of gammas in each regression
  int<lower=1> idexi [sum(ngamma)];   // factors explaining exogenous identifier
  int<lower=1> idex[Nex];             // (dependent) exogenous identifier
  int<lower=1> idlambex [sum(ngamma)]; // scores identifier

   int<lower=1> asc [Nob];
   int<lower=1> asc2 [Nna];
   matrix[Nv-Nex,K] v;

  // prior specification
  real<lower=0> dsigma2 [Nv-Nex];
  real a [Nv-Nex];
  real<lower=0> b [Nv-Nex];
  real<lower=0> dtau2 [Nex];
  real at [Nex];
  real<lower=0> bt [Nex];

  real<lower=0> dbeta [sum(nbeta)];
  real<lower=0> m [sum(nbeta)];
  real<lower=0> s [sum(nbeta)];
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
  vector[Nna] Xna;

  row_vector[sum(nbeta)] beta;

  row_vector[Nex] gamma0;
  row_vector[sum(ngamma)] gamma;
  row_vector<lower=0> [Nex] tau2;
}

model{

  for(i in 1:Nob){
    X[idob[i,1],idob[i,2]] ~ normal(alpha[asc[i],] * lambda[,idob[i,2]], sqrt(sigma2[asc[i]]));
  }

  // Loadings prior
   for(k in 1:K){
      alpha[,k] ~ normal(0, sqrt(v[,k]));
    }

   lambda[idy[1],] ~ normal(beta[1:nbeta[1]]*lambda[idlamb[1:nbeta[1]],], 1);

  if(Ny>1){
    for(i in 2:Ny){
     lambda[idy[i],] ~ normal(beta[(sum(nbeta[1:(i-1)])+1):sum(nbeta[1:i])]*lambda[idlamb[(sum(nbeta[1:(i-1)])+1):sum(nbeta[1:i])],], 1);
    }
  }

 // independet lambda prior
  to_vector(lambda[idyi,]) ~ normal(0, 1);


  // exogenous regression
   X[idex[1],] ~ normal(gamma0[1] + gamma[1:ngamma[1]]*lambda[idlambex[1:ngamma[1]],], sqrt(tau2[1]));

  if(Nex>1){
    for(i in 2:Nex){
     X[idex[i],] ~ normal(gamma0[i] + gamma[(sum(ngamma[1:(i-1)])+1):sum(ngamma[1:i])]*lambda[idlambex[(sum(ngamma[1:(i-1)])+1):sum(ngamma[1:i])],], sqrt(tau2[i]));
    }
  }

  // missing data prior
  for(i in 1:Nna){
    Xna[i] ~ normal(alpha[asc2[i],] * lambda[,idna[i,2]], sqrt(sigma2[asc2[i]]));
  }

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
  for(i in 1:sum(nbeta)){
    // coefficients prior and regression
    if(dbeta[i] == 0){
      beta[i] ~ normal(m[i], sqrt(s[i]));
    }
    else{
      beta[i] ~ cauchy(m[i], sqrt(s[i]));
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
//empty line avoids crash
