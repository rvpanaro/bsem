
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
  matrix[Nv,K] v;

  real<lower=0> dsigma2 [Nv];
  real a [Nv];
  real<lower=0> b [Nv];

  real<lower=0> dbeta [sum(nbeta)];
  real<lower=0> m [sum(nbeta)];
  real<lower=0> s [sum(nbeta)];
}

parameters{
  matrix[Nv,K] alpha;
  matrix[K,Ne] lambda;
  vector<lower=0> [Nv] sigma2;
  vector[Nna] Xna;

  row_vector[sum(nbeta)] beta;
}

model{

  for(i in 1:Nob){
    X[idob[i,1],idob[i,2]] ~ normal(alpha[idob[i,1],] * lambda[,idob[i,2]], sqrt(sigma2[idob[i,1]]));
  }

  // Loadings prior
   for(k in 1:K){
      alpha[,k] ~ normal(0, sqrt(v[,k]));
    }

   lambda[idy[1],] ~ normal(beta[1:nbeta[1]]*lambda[idlamb[1:nbeta[1]],], 1);

  for(i in 2:Ny){
     lambda[idy[i],] ~ normal(beta[(sum(nbeta[1:(i-1)])+1):sum(nbeta[1:i])]*lambda[idlamb[(sum(nbeta[1:(i-1)])+1):sum(nbeta[1:i])],], 1);
    }

  // lambda prior
  to_vector(lambda[idyi,]) ~ normal(0, 1);

  // missing data prior
  for(i in 1:Nna){
    Xna[i] ~ normal(alpha[idna[i,1],] * lambda[,idna[i,2]], sqrt(sigma2[idna[i,1]]));
  }


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

  for(i in 1:sum(nbeta)){
    // coefficients prior and regression
    if(dbeta[i] == 0){
      beta[i] ~ normal(m[i], sqrt(s[i]));
    }
    else{
      beta[i] ~ cauchy(m[i], sqrt(s[i]));
    }
  }
}
//empty line avoids crash
