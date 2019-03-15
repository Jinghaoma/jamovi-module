data {
	        int<lower=0>  N1;
          int<lower=0>  N2;
          real X1[N1];
          real X2[N2];
}
parameters {
          real          mu1;
          real          mu2;
          real<lower=0> sigma1;
          real<lower=0> sigma2;

          real          mu1_same;
          real          mu2_same;
          real<lower=0> sigma_same;
}
model {
          X1 ~ normal(mu1,sigma1);
          X2 ~ normal(mu2,sigma2);

          X1 ~ normal(mu1_same,sigma_same);
          X2 ~ normal(mu2_same,sigma_same);
}

