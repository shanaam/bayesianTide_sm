
  data {
    int N;   // number of observations
    vector[N] Coffee;   
    vector[N] Stress;   
    vector[N] Heart;
  }
  parameters {
    real b_0;      // intercept parameter
    real b_Coffee; 
    real b_Stress;
    real<lower=0> sigma_y; // non-negative standard deviation
  }
  model {
    Heart ~ normal(
      b_0 + b_Coffee * Coffee + b_Stress * Stress, // scalar *or+ vector = vector
      sigma_y);   // model of form y ~ normal(mean, sd) 
  }
