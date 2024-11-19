data {
    int<lower=0> n_obs;
    array[n_obs] int resp; // response
    array[n_obs] int site;
}

parameters {
   real beta_0;
   real mean;
   vector[2] site_effect;
   real phi;
}


model {
    vector[n_obs] eta 

    for(n in 1:n_obs) {
        eta[n] = mean + site_effect[site[n]];
    }

    for(i in 1:n_sites) {

    }

    // likelihood
    y ~ neg_binomial_2_log(eta, phi);

}