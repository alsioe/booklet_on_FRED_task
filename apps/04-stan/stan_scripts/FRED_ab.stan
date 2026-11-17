// Non hierarchical ab model
// for single-session FRED task

data {
  
  // Number of trials
  int<lower=1> n_trials;
  int<lower=1> ratio[n_trials];
  int<lower=0, upper=1> chose_large[n_trials];
  
  // Priors
  real alpha_prior_mean;
  real alpha_prior_sd;
  real beta_prior_shape;
  real beta_prior_scale;

}

// The parameters accepted by the model.
// Our model accepts alpha and beta
parameters {
  real alpha;
  real<lower=0> beta;
}

model {

  // priors
  alpha ~ normal(alpha_prior_mean, alpha_prior_sd);
  beta ~ gamma(beta_prior_shape, beta_prior_scale);

  // create overall structures for trial-wise values
  vector[n_trials] value_large;
  vector[n_trials] prob_large;

  // loop the loop
  for (t in 1:n_trials) {
    
    // SUBJECTIVE VALUE
    value_large[t] = - beta * (ratio[t] - alpha);
    
    // ACTION SELECTION
    prob_large[t] = 1 / (1 + exp(- value_large[t]));
    
  }
  
  chose_large ~ bernoulli(prob_large);
  
}

generated quantities {
    
  // log_lik SHOULD SAVE TO THE FIT OBJECT
  vector[n_trials] log_lik;
  
  { // DO NOT SAVE LOCAL VARIABLES
  // create overall structures for trial-wise values
  vector[n_trials] value_large;
  vector[n_trials] prob_large;

  // loop the loop
  for (t in 1:n_trials) {
    
    // SUBJECTIVE VALUE
    value_large[t] = - beta * (ratio[t] - alpha);
    
    // ACTION SELECTION
    prob_large[t] = 1 / (1 + exp(- value_large[t]));
    
    log_lik[t] = bernoulli_lpmf(chose_large[t] | prob_large[t]);    
    
  } // End of for loop
  
  } // End of local environment that does not save
  
}
