######################################X
#---Community Ecology Final Project---X
#--------------Fall 2022--------------X
#------------Brian J. Smith-----------X
######################################X

# Load packages ----
library(dplyr)
library(tidyr)

# Custom functions ----
source("99_fun.R")

# Load data ----
# Input data
dat <- read.csv("out/combined_data.csv")

# MCMC samples
np1 <- read.csv("out/MCMC_samples_no_pred.csv")
np2 <- read.csv("out/MCMC_samples2_no_pred.csv")

w1 <- read.csv("out/MCMC_samples_wolf.csv")
w2 <- read.csv("out/MCMC_samples2_wolf.csv")

wc1 <- read.csv("out/MCMC_samples_wolf_cougar.csv")
wc2 <- read.csv("out/MCMC_samples2_wolf_cougar.csv")

# No predators ----
np_sim <- sim_LV(N_t1 = c(dat$elk[nrow(dat)], dat$bison[nrow(dat)]),
                 P_t = c(0, 0), 
                 r = c(mean(np1$r_elk), mean(np1$r_bison)),
                 alpha = matrix(c(mean(np1$alpha_ee), 
                                  mean(np1$alpha_eb),
                                  mean(np1$alpha_be), 
                                  mean(np1$alpha_bb)),
                                nrow = 2, ncol = 2, byrow = TRUE),
                 T = 100) %>% 
  as.data.frame() %>% 
  mutate(model = "np",
         year = seq(2018, by = 1, length.out = nrow(.)))

# Wolves only ----
w_sim <- sim_LV(N_t1 = c(dat$elk[nrow(dat)], dat$bison[nrow(dat)]),
                P_t = c(50, 0), 
                r = c(mean(w1$r_elk), mean(w1$r_bison)),
                alpha = matrix(c(mean(w1$alpha_ee), 
                                 mean(w1$alpha_eb),
                                 mean(w1$alpha_be), 
                                 mean(w1$alpha_bb)),
                               nrow = 2, ncol = 2, byrow = TRUE),
                beta = c(mean(w1$beta_elk), mean(w1$beta_bison)),
                T = 100) %>% 
  as.data.frame() %>% 
  mutate(model = "w",
         year = seq(2018, by = 1, length.out = nrow(.)))

# Wolves and cougars ----
wc_sim <- sim_LV(N_t1 = c(dat$elk[nrow(dat)], dat$bison[nrow(dat)]),
                P_t = c(50, 25), 
                r = c(mean(wc1$r_elk), mean(wc1$r_bison)),
                alpha = matrix(c(mean(wc1$alpha_ee), 
                                 mean(wc1$alpha_eb),
                                 mean(wc1$alpha_be), 
                                 mean(wc1$alpha_bb)),
                               nrow = 2, ncol = 2, byrow = TRUE),
                beta = c(mean(wc1$beta_elk), mean(wc1$beta_bison)),
                gamma = mean(wc1$gamma_elk),
                T = 100) %>% 
  as.data.frame() %>% 
  mutate(model = "wc",
         year = seq(2018, by = 1, length.out = nrow(.)))

# Combine ----
sims <- rbind(np_sim,
              w_sim,
              wc_sim)

# Save ----
write.csv(sims, "out/simulations.csv", row.names = FALSE)
