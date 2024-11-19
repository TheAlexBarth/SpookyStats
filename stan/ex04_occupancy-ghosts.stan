data {
    int<lower=0> N; //houses
    array[N] int<lower=0> v; // visits
    array[N] int<lower=0> detections;
    vector[N] x;
    vector[N] c;
}

parameters {
    real beta_0;
    real beta_1;
    real alpha_0;
    real alpha_1;
}

transformed parameters {
    array[N] real p;
    array[N] real psi;

    for(i in 1:N) {
        p[i] = inv_logit(alpha_0 + alpha_1 * c[i]);
        psi[i] = inv_logit(beta_0 + beta_1 * x[i]);
    }
}

model {
    // Priors
    beta_0 ~ normal(-10, 3);
    beta_1 ~ normal(0, 2);
    alpha_0 ~ normal(0, 10);
    alpha_1 ~ normal(0, 2);


    for (i in 1:N) {
        detections[i] ~ binomial(v[i], p[i] * psi[i]);
    }
}
