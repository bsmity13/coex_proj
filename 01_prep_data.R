######################################X
#---Community Ecology Final Project---X
#--------------Fall 2022--------------X
#------------Brian J. Smith-----------X
######################################X

# Load packages ----
library(dplyr)

# Load data ----
elk <- read.csv("data/elk_estimates_Tallian_model_BJS_2022-07-26.csv")
bison <- read.csv("data/NR_bison_estimated.csv")
wolf <- read.csv("data/NR_wolf_population.csv")
cougar <- read.csv("data/NR_cougar_density.csv")

# Process data ----
# Elk
e <- elk %>% 
  mutate(elk = round(mean)) %>% 
  select(year, elk) %>% 
  filter(year %in% 1970:2018)

# Bison
b <- bison %>% 
  select(year = Year, bison = Bison) %>% 
  # Assume bison were ~ 300 from 1970 -- 1977
  rbind(data.frame(year = 1970:1977, bison = 300)) %>% 
  filter(year %in% 1970:2018) %>% 
  arrange(year)

# Wolves
w <- wolf %>% 
  select(year = winter, wolf = wolf) %>% 
  # Wolves were 0 before 1994
  rbind(data.frame(year = 1970:1993, wolf = 0)) %>% 
  arrange(year) %>% 
  filter(year %in% 1970:2018)

# Cougars
c <- cougar %>% 
  # Assume density of 0.6 for all years prior to 1987
  rbind(data.frame(winter = 1970:1986, cougar = 0.6, interpolated = TRUE)) %>% 
  mutate(cougar = round(cougar * 18)) %>% 
  select(year = winter, cougar) %>% 
  filter(year %in% 1970:2018) %>% 
  arrange(year)

# Combine ----
dat <- e %>% 
  left_join(b, by = "year") %>% 
  left_join(w, by = "year") %>% 
  left_join(c, by = "year") %>% 
  mutate(t = as.numeric(factor(year)))

# Save ----
write.csv(dat, "out/combined_data.csv", row.names = FALSE)
