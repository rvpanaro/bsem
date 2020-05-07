data{
  int<lower=1> K;
  int<lower=1> Nv;
  int<lower=1> Ne;
  int<lower=1> Nob;
  int<lower=1> idob[Nob,2];
  int<lower=1> Ny;                  // n of dependent scores
  int<lower=1> idyi[K-Ny];          // independent scores identifier
  int<lower=1> idy[Ny];             // dependent scores identifier
  int<lower=1> nbeta [Ny];          // number of betas in each regression
  int<lower=1> idlamb [sum(nbeta)]; // scores identifier
  matrix[Nv,Ne] X;
  matrix[Nv,K] v;
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> s;
  //////////////////
}

parameters{
  matrix[Nv,K] alpha;
  matrix[K, Ne] lambda;
  vector[Nv] sigma2;

  row_vector[sum(nbeta)] beta;
}

model{
  for(i in 1:Nob){
    X[idob[i,1],idob[i,2]] ~ normal(alpha[idob[i,1],] * lambda[,idob[i,2]], sqrt(sigma2[idob[i,1]]));
  }

  // dependent scores distribution
  lambda[idy[1],] ~ normal(beta[1:nbeta[1]]*lambda[idlamb[1:nbeta[1]],], 1);

  // Loadings prior
   for(k in 1:K){
      alpha[,k] ~ normal(0, sqrt(v[,k]));
    }

  for(i in 2:Ny){
      lambda[idy[i],] ~ normal(beta[(sum(nbeta[1:(i-1)])+1):sum(nbeta[1:i])]*lambda[idlamb[(sum(nbeta[1:(i-1)])+1):sum(nbeta[1:i])],], 1);
   }

  // independent scores prior
  to_vector(lambda[idyi,]) ~ normal(0, 1);

  // sigma2 prior
  sigma2 ~ inv_gamma(a, b);

  // coefficients prior and regression
  beta ~ normal(0, sqrt(s));
}
// empty line avoids crash

