######################################X
#---Community Ecology Final Project---X
#--------------Fall 2022--------------X
#------------Brian J. Smith-----------X
######################################X

# Load packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ragg)

# Colors ----
e_col <- "green3"
b_col <- "rosybrown"
w_col <- "gray40"
c_col <- "gold"

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

# Simulations
sim <- read.csv("out/simulations.csv")

# Timeseries ----
herb_ts <- dat %>% 
  select(year, elk, bison) %>% 
  pivot_longer(elk:bison, names_to = "species", values_to = "n") %>% 
  ggplot(aes(x = year, y = n, color = species)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("N") +
  ggtitle("Herbivores") +
  scale_color_manual(name = "Herbivore",
                     breaks = c("elk", "bison"),
                     values = c(e_col, b_col)) +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.background = element_rect(fill = "magenta",
                                       color = "magenta")) +
  NULL

pred_ts <- dat %>% 
  select(year, wolf, cougar) %>% 
  pivot_longer(wolf:cougar, names_to = "species", values_to = "n") %>% 
  ggplot(aes(x = year, y = n, color = species)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("N") +
  ggtitle("Predators") +
  scale_color_manual(name = "Predator",
                     breaks = c("cougar", "wolf"),
                     values = c(c_col, w_col)) +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.background = element_rect(fill = "magenta",
                                       color = "magenta")) +
  NULL

# Combine
dat_ts <- herb_ts / pred_ts &
  plot_annotation(theme = 
                    theme(plot.background = element_rect(fill = "magenta",
                                                         color = "magenta")))

# Save 
ggsave("fig/data_timeseries.tif", plot = dat_ts, device = agg_tiff,
       width = 8.75, height = 11, units = "in", compression = "lzw",
       bg = "green")

# Coexistence ----
coex_np <- np2 %>% 
  mutate(model = "No Predators") %>% 
  select(model, elk_win, bison_win, stable_coex, unstable) %>% 
  pivot_longer(elk_win:unstable,
               names_to = "outcome",
               values_to = "prob") %>% 
  group_by(model, outcome) %>% 
  summarize(mean = mean(prob),
            lwr = quantile(prob, 0.025),
            upr = quantile(prob, 0.975))

coex_w <- w2 %>% 
  mutate(model = "Wolf Only") %>% 
  select(model, elk_win, bison_win, stable_coex, unstable) %>% 
  pivot_longer(elk_win:unstable,
               names_to = "outcome",
               values_to = "prob") %>% 
  group_by(model, outcome) %>% 
  summarize(mean = mean(prob),
            lwr = quantile(prob, 0.025),
            upr = quantile(prob, 0.975))

coex_wc <- wc2 %>% 
  mutate(model = "Wolf and Cougar") %>% 
  select(model, elk_win, bison_win, stable_coex, unstable) %>% 
  pivot_longer(elk_win:unstable,
               names_to = "outcome",
               values_to = "prob") %>% 
  group_by(model, outcome) %>% 
  summarize(mean = mean(prob),
            lwr = quantile(prob, 0.025),
            upr = quantile(prob, 0.975))

coex <- rbind(coex_np, coex_w, coex_wc) %>% 
  mutate(model = factor(model, levels = c("No Predators",
                                          "Wolf Only",
                                          "Wolf and Cougar")),
         outcome = factor(outcome, levels = c("elk_win", "bison_win",
                                              "unstable", "stable_coex"))) %>% 
  mutate(across(mean:upr, round, digits = 2)) %>% 
  mutate(txt = format(mean, nsmall = 2))


# Plot
coex_plot <- ggplot(coex, aes(x = model, y = mean, 
                              fill = outcome)) +
  geom_bar(stat = "identity", 
           position=position_dodge(preserve = "single"),
           color = "black") +
  geom_text(aes(label = txt), position = position_dodge(width = 0.95), 
            vjust = -0.5, size = 5) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(breaks = c("elk_win", "bison_win" ,"unstable", "stable_coex"),
                    labels = c("Elk Win", "Bison Win", "Unstable", "Stable Coex."),
                    values = c(e_col, b_col, "firebrick", "#2297E6")) +
  xlab("Model") +
  ylab("Pr(outcome)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.background = element_rect(fill = "magenta",
                                       color = "magenta"),
        legend.background = element_rect(fill = "white",
                                         color = "magenta")) +
  NULL

ggsave("fig/coexistence.tif", plot = coex_plot, device = agg_tiff,
       width = 11, height = 12, units = "in", compression = "lzw",
       bg = "green")

# Simulations ----
sim2 <- sim %>% 
  pivot_longer(N_elk:N_bison,
               names_to = "species",
               values_to = "N")

(np_sim_plot <- sim2 %>% 
    filter(model == "np") %>% 
    ggplot(aes(x = year, y = N, color = species)) +
    geom_line(size = 1.5) +
    xlab("Year") +
    ylab("N") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_x_continuous(breaks = c(2018, 2040, 2060, 2080, 2100, 2117)) +
    scale_color_manual(name = "Population",
                       breaks = c("N_elk", "N_bison"),
                       labels = c("elk", "bison"),
                       values = c(e_col, b_col)) +
    ggtitle("No Predators", subtitle = "Wolf = 0, Cougar = 0") +
    theme_bw() +
    theme(text = element_text(size = 24),
          plot.background = element_rect(fill = "magenta",
                                         color = "magenta")) +
    NULL)

(w_sim_plot <- sim2 %>% 
    filter(model == "w") %>% 
    ggplot(aes(x = year, y = N, color = species)) +
    geom_line(size = 1.5) +
    xlab("Year") +
    ylab("N") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_x_continuous(breaks = c(2018, 2040, 2060, 2080, 2100, 2117)) +
    scale_color_manual(name = "Population",
                       breaks = c("N_elk", "N_bison"),
                       labels = c("elk", "bison"),
                       values = c(e_col, b_col)) +
    ggtitle("Wolf Only", subtitle = "Wolf = 50, Cougar = 0") +
    theme_bw() +
    theme(text = element_text(size = 24),
          plot.background = element_rect(fill = "magenta",
                                         color = "magenta")) +
    NULL)

(wc_sim_plot <- sim2 %>% 
    filter(model == "wc") %>% 
    ggplot(aes(x = year, y = N, color = species)) +
    geom_line(size = 1.5) +
    xlab("Year") +
    ylab("N") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_x_continuous(breaks = c(2018, 2040, 2060, 2080, 2100, 2117)) +
    scale_color_manual(name = "Population",
                       breaks = c("N_elk", "N_bison"),
                       labels = c("elk", "bison"),
                       values = c(e_col, b_col)) +
    ggtitle("Wolf and Cougar", subtitle = "Wolf = 50, Cougar = 25") +
    theme_bw() +
    theme(text = element_text(size = 24),
          plot.background = element_rect(fill = "magenta",
                                         color = "magenta")) +
    NULL)

# Combine
sim_plot <- ((np_sim_plot + theme(legend.position = "none")) / 
               w_sim_plot / 
               (wc_sim_plot + theme(legend.position = "none"))) &
  plot_annotation(theme = 
                    theme(plot.background = element_rect(fill = "magenta",
                                                         color = "magenta")))

# Save 
ggsave("fig/simulations.tif", plot = sim_plot, device = agg_tiff,
       width = 8.75, height = 13, units = "in", compression = "lzw",
       bg = "green")
