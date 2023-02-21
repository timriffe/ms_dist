
# These data come from Foltyn & Olsson 2021,

# we just need to extend to age 110+, ALR seems to work well

library(tidyverse)
library(compositions)

p_tibble2 <- 
  read_csv("https://raw.githubusercontent.com/richardfoltyn/health-process/master/Health-2/CSV/H2_trans_prob_age50-99_female_nonblack.csv") %>% 
  filter(health != 0) %>% 
  mutate(from = ifelse(health == 1, "H", "U")) %>% 
  pivot_longer(3:5, names_to = "to", values_to = "p") %>% 
  mutate(to = case_when(to == "Health1" ~ "H",
                        to == "Health2" ~ "U",
                        TRUE ~ "D")) %>% 
  select(-health) %>% 
  mutate(from_to = paste0(from,to)) %>% 
  select(-from, -to) %>% 
  pivot_wider(names_from = from_to, values_from = p)

extrap_alr <- function(y){
  x <- 50:99
  xnew <- data.frame(x=100:110)
  y2 = predict(lm(y~x), newdata = xnew)
  c(y,y2)
}

fromH <-
  p_tibble2 %>% 
  select(HH, HU, HD) %>% 
  as.matrix() %>% 
  alr() %>% 
  apply(2,extrap_alr) %>% 
  alrInv() %>% 
  as.data.frame() %>% 
  rename(HD = V3) 

fromU <-
  p_tibble2 %>% 
  select(UH, UU, UD) %>% 
  as.matrix() %>% 
  alr() %>% 
  apply(2,extrap_alr) %>% 
  alrInv() %>% 
  as.data.frame() %>% 
  rename(UD = V3)

bind_cols(fromH, fromU) %>% 
  mutate(age = 50:110, .before = 1) %>% 
  remove_rownames() %>% 
  write_csv("transitions_extrap_hrs.csv")





