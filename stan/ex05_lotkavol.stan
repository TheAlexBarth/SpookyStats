functions {
    array[] real dz_dt(
        real t, 
        array[] real z,
        array[] real theta,
        array[] real x_r,
        array[] int x_i) {
        real g = z[1];
        real b = z[2];

        real alpha = theta[1];
        real beta = theta[2];
        real gamma = theta[3];
        real delta = theta[4];

        real dg_dt = (alpha - beta * b) * g;
        real db_dt = (-gamma + delta * g) * b;
        return {dg_dt, db_dt};
    }
}



data {
  int<lower = 0> step;
  int<lower=0> b_obs;
  int<lower=0> g_obs;
  array[step] real<lower = 0> T; //resolution
  array[b_obs] int b_times; //obs times
  array[g_obs] int g_times;
  real b_init; //initial buster observation 
  array[b_obs] real<lower = 0> b; // timed obs
  array[g_obs] real<lower = 0> g; //timed obs for g
}

parameters {
  array[4] real<lower = 0> theta; // param vector
  array[2] real<lower = 0> z_init; // initial states
  real b_sigma; // observation error
  real g_sigma; // ghost error
}

transformed parameters {
   // population for remaining years
   array[step, 2] real z = integrate_ode_rk45(dz_dt, z_init, 0, T, theta,
                                rep_array(0.0, 0), rep_array(0, 0),
                                1e-6, 1e-5, 1e3);
}

model {
   // priors
   b_sigma ~ normal(0, 0.1);
   g_sigma ~ normal(2, 0.1);
   theta[1] ~ normal(0.1, 0.00005);
   theta[2] ~ normal(0.002, 0.00001);
   theta[3] ~ normal(0.002, 0.00005);
   theta[4] ~ normal(0.1, 0.0001);
   z_init[1] ~ normal(120, 10);
   z_init[2] ~ normal(b_init, b_sigma); // we do know this one

   //observation process
   for(t in 1:b_obs) {
    b[t] ~ normal(z[b_times[t], 2], b_sigma);
   }
   for(t in 1:g_obs) {
    g[t] ~ normal(z[g_times[t], 1], g_sigma);
   }
 }