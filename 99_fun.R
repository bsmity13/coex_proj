######################################X
#---Community Ecology Final Project---X
#--------------Fall 2022--------------X
#------------Brian J. Smith-----------X
######################################X

# Functions

# LV model for one timestep ----
# Parameters:
#   -- N: a vector of length 4, i.e., c(N_elk, N_bison)
#   -- P: a vector of length 2, i.e., c(N_wolf, N_cougar)
#   -- r: a vector of length 2, i.e., c(r_elk, r_bison)
#   -- alpha: a 2x2 matrix; first subscript gives row, second column
#       -- e.g., alpha[1,2] corresponds to \alpha_{1,2} (see examples below)
#       -- 1 is elk; 2 is bison
#   -- beta: a vector of length 2, i.e., c(beta_elk, beta_bison)
#   -- gamma: a vector of length 1, i.e., gamma_elk

LV <- function(N, P = c(0, 0), r, alpha, beta = c(0, 0), gamma = 0) {
  # N_elk at t + 1
  N_elk_tp1 <- N[1] + r[1] * N[1] * (1 - 
                                    alpha[1,1] * N[1] - 
                                    alpha[1,2] * N[2]) -
    beta[1] * N[1] * P[1] -
    gamma[1] * N[1] * P[2]
  
  # N2 at t+1
  N_bison_tp1 <- N[2] + r[2] * N[2] * (1 - 
                                    alpha[2,1] * N[1] - 
                                    alpha[2,2] * N[2]) -
    beta[2] * N[2] * P[1]
  
  # Return
  # Returns vector of length 2
  return(c(N_elk_tp1, N_bison_tp1))
}

# # Examples
# 
# # Structure of alpha
# matrix(c("a11", "a12", "a21", "a22"), nrow = 2, ncol = 2, byrow = TRUE)
# matrix(c("alpha_ee", "alpha_eb", 
#          "alpha_be", "alpha_bb"), 
#        nrow = 2, ncol = 2, byrow = TRUE,
#        dimnames = list(c("e", "b"), c("e", "b")))
# 
# # Example run
# LV(N = c(5, 6),
#    r = c(0.02, 0.02),
#    alpha = matrix(c(0.01, 0.05, 0.05, 0.012),
#                   nrow = 2, ncol = 2, byrow = TRUE))

# simulate LV timeseries ----

# Parameters:
#   -- N_t1: a vector of length 2 giving c(N_elk, N_bison) at t = 1
#   -- P_t : either:
#         -- a T x 2 matrix giving cbind(N_wolf, N_cougar) for all t
#         -- a vector of length 2 giving c(N_wolf, N_cougar) to be repeated
#             for all t
#   -- r: same as 'LV()'
#   -- alpha: same as 'LV()'
#   -- T: number of timesteps to run the model
#   -- ext_cut: extinction cutoff; any N below this is set to 0 [default = 0.001]

sim_LV <- function(N_t1, P_t = c(0, 0), r, alpha, 
                   beta = c(0, 0), gamma = 0, T, ext_cut = 0.5) {
  # Setup matrix to hold result
  # Will be T x 2 matrix 
  #   i.e., rows are time, columns are species1 and species2
  N <- matrix(NA, nrow = T, ncol = 2)
  colnames(N) <- c("N_elk", "N_bison")
  rownames(N) <- paste0("t", 1:T)
  
  # Setup predator matrix
  if (!is.matrix(P_t)) {
    P <- matrix(nrow = T, ncol = 2)
    P[, 1] <- P_t[1]
    P[, 2] <- P_t[2]
  } else {
    P <- P_t
  }
  
  # Plug initial N into matrix
  N[1, ] <- N_t1
  
  # Loop to run simulation
  for (t in 2:T) {
    # LV
    N[t, ] <- LV(N = N[(t-1), ], 
                 P = P[(t-1), ],
                 r = r, 
                 alpha = alpha,
                 beta = beta,
                 gamma = gamma)
    # Apply extinction cut-off
    N[N < ext_cut] <- 0
  }
  
  # Return
  return(N)
}

# Examples
# 
# sim_LV(N_t1 = c(5, 6),
#        r = c(0.01, 0.05),
#        alpha = matrix(c(0.01, 0.05, 0.05, 0.012),
#                       nrow = 2, ncol = 2, byrow = TRUE),
#        T = 10)
# 
# sim_LV(N_t1 = c(5000, 600),
#        P_t = c(50, 25),
#        r = c(0.289, 0.368),
#        alpha = matrix(c(5.54e-5, 
#                         5.30e-5, 
#                         2.17e-5, 
#                         1.73e-4),
#                       nrow = 2, ncol = 2, byrow = TRUE),
#        beta = c(0.0019, 7.96e-5),
#        gamma = c(0.0018),
#        T = 100)

# matrix(c("alpha_ee", "alpha_eb",
#          "alpha_be", "alpha_bb"),
#        nrow = 2, ncol = 2, byrow = TRUE,
#        dimnames = list(c("e", "b"), c("e", "b")))


