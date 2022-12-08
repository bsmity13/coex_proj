######################################X
#---Community Ecology Final Project---X
#--------------Fall 2022--------------X
#------------Brian J. Smith-----------X
######################################X

# Load packages ----
library(dplyr)
library(nimble)

# Load data ----
dat <- read.csv("out/combined_data.csv")

# Format data for NIMBLE ----
# Data
mod_dat <- list(N_elk = dat$elk,
                N_bison = dat$bison)

# Constants
mod_const <- list(length_t = nrow(dat))

# Initial values
mod_inits <- list(r_elk = 1,
                  r_bison = 1,
                  alpha_ee = 0.00001,
                  alpha_eb = 0.00001,
                  alpha_bb = 0.00001,
                  alpha_be = 0.00001,
                  mu_elk = dat$elk,
                  mu_bison = dat$bison)

# Model specification ----
mod_spec <- nimbleCode({
  ## ... Priors ----
  # Growth rate
  r_elk ~ dnorm(0, sd = 0.3)
  r_bison ~ dnorm(0, sd = 0.3)
  
  # Competition
  alpha_ee ~ dbeta(1, 1)
  alpha_eb ~ dbeta(1, 1)
  alpha_bb ~ dbeta(1, 1)
  alpha_be ~ dbeta(1, 1)
  
  ## ... Likelihood ----
  # Elk timeseries
  for (t in 2:length_t) {
    mu_elk[t] <- N_elk[t - 1] + r_elk * N_elk[t - 1] * (1 - 
                                                          alpha_ee * N_elk[t - 1] - 
                                                          alpha_eb * N_bison[t - 1])
    
    
    mu_bison[t] <- N_bison[t - 1] + r_bison * N_bison[t - 1] * (1 - 
                                                                  alpha_bb * N_bison[t - 1] - 
                                                                  alpha_be * N_elk[t - 1])
    
    
    N_elk[t] ~ dpois(mu_elk[t])
    N_bison[t] ~ dpois(mu_bison[t])
    
  }
  
  ## ... derived quantities ----
  # Elk win:
  elk_win <- ((alpha_bb/alpha_be) < (alpha_bb/alpha_ee)) & ((alpha_bb/alpha_ee) > (alpha_eb/alpha_ee))
  
  # Bison win:
  bison_win <- ((alpha_bb/alpha_be) > (alpha_bb/alpha_ee)) & ((alpha_bb/alpha_ee) < (alpha_eb/alpha_ee))
  
  # Stable coexistence:
  stable_coex <- ((alpha_bb/alpha_be) > (alpha_bb/alpha_ee)) & ((alpha_bb/alpha_ee) > (alpha_eb/alpha_ee))
  
  # Unstable equilibrium:
  unstable <- ((alpha_bb/alpha_be) < (alpha_bb/alpha_ee)) & ((alpha_bb/alpha_ee) < (alpha_eb/alpha_ee))
  
})

# Create NIMBLE objects ---- 
# Main model object
mod <- nimbleModel(code = mod_spec,
                   data = mod_dat,
                   constants = mod_const,
                   inits = mod_inits,
                   calculate = TRUE)

# Configure MCMC
mod_conf <- configureMCMC(mod,
                          monitors2 = c("mu_elk", "mu_bison", 
                                        "elk_win", "bison_win", 
                                        "stable_coex", "unstable"))

# Build MCMC
mod_mcmc <- buildMCMC(mod_conf)

# Compile model
C_mod <- compileNimble(mod)

# Compile MCMC
C_mcmc <- compileNimble(mod_mcmc)

# Run MCMC ----
C_mcmc$run(niter = 50000,
           nburnin = 20000,
           thin = 3,
           thin2 = 3,
           reset = TRUE)

# Get samples
samples <- as.matrix(C_mcmc$mvSamples)
samples2 <- as.matrix(C_mcmc$mvSamples2)

# Traceplots ----
# r_elk
plot(1:nrow(samples), samples[,"r_elk"], type = "l")

# r_bison
plot(1:nrow(samples), samples[,"r_bison"], type = "l")

# alpha_ee
plot(1:nrow(samples), samples[,"alpha_ee"], type = "l")

# alpha_eb
plot(1:nrow(samples), samples[,"alpha_eb"], type = "l")

# alpha_bb
plot(1:nrow(samples), samples[,"alpha_bb"], type = "l")

# alpha_be
plot(1:nrow(samples), samples[,"alpha_be"], type = "l")

## Competition outcome
# elk_win
plot(1:nrow(samples2), samples2[,"elk_win"], type = "l")
mean(samples2[, "elk_win"])

# bison_win
plot(1:nrow(samples2), samples2[,"bison_win"], type = "l")
mean(samples2[, "bison_win"])

# stable_coex
plot(1:nrow(samples2), samples2[,"stable_coex"], type = "l")
mean(samples2[, "stable_coex"])

# unstable
plot(1:nrow(samples2), samples2[,"unstable"], type = "l")
mean(samples2[, "unstable"])

# Population means ----
mu_bison_samps <- samples2[, grep("mu_bison", colnames(samples2))]
mu_elk_samps <- samples2[, grep("mu_elk", colnames(samples2))]

mu_bison_mean <- apply(mu_bison_samps, 2, mean, na.rm = TRUE)
mu_elk_mean <- apply(mu_elk_samps, 2, mean, na.rm = TRUE)

# Goodness of fit:
plot(dat$elk[-1], mu_elk_mean[-1])
abline(a = 0, b = 1, col = "red")

plot(dat$bison[-1], mu_bison_mean[-1])
abline(a = 0, b = 1, col = "red")

# Derived quantities ----
# Elk carrying capacity
1/mean(samples[, "alpha_ee"])

# Bison carrying capacity
1/mean(samples[, "alpha_bb"])

# Save ----
write.csv(samples, "out/MCMC_samples_no_pred.csv", row.names = FALSE)
write.csv(samples2, "out/MCMC_samples2_no_pred.csv", row.names = FALSE)


