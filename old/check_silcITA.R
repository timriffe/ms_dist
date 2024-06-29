library(tidyverse)
p45 <- read_csv("pij45.csv",
                show_col_types = FALSE) %>% 
  rename(HH = p11,
         HU = p12,
         HD = p13,
         UU = p22,
         UH = p21,
         UD = p23,
         age = Age1) %>% 
  mutate(variant = "p45")
p455 <- read_csv("pij455.csv",
                 show_col_types = FALSE) %>% 
  rename(HH = p11,
         HU = p12,
         HD = p13,
         UU = p22,
         UH = p21,
         UD = p23,
         age = Age1) %>% 
  mutate(variant = "p455")
p46 <- read_csv("pij46.csv",
                show_col_types = FALSE) %>% 
  rename(HH = p11,
         HU = p12,
         HD = p13,
         UU = p22,
         UH = p21,
         UD = p23,
         age = Age1) %>% 
  mutate(variant = "p46")

bind_rows(p45,p455,p46) %>% 
  pivot_longer(-c(age,variant),names_to = "from_to",values_to = "p") %>% 
  ggplot(aes(x = age, y = p, color = from_to, linetype = variant)) + 
  geom_line() +
  theme_minimal()

qx <- HMDHFDplus::readHMDweb("ITA","mltper_1x1",
                             username = Sys.getenv("us"),
                             password = Sys.getenv("pw")) %>% 
  filter(Year == 2017) %>% 
  select(age = Age, qx) %>% 
  mutate(from = "all cause (HMD)")

p455 %>% 
  select(age, HD, UD) %>% 
  pivot_longer(-age, names_to = "from", values_to = "qx") %>% 
  bind_rows(qx) %>% 
  filter(between(age, 50,109)) %>% 
  ggplot(aes(x = age, y = qx, color = from)) +
  geom_line() +
  theme_minimal()

p455S <-
  p455 %>% 
  mutate(HHS = HH / (1-HD),
         HUS = HU / (1-HD),
         UUS = UU / (1-UD),
         UHS = UH / (1-UD)) %>% 
  select(age, HHS, HUS, UUS, UHS) %>% 
  pivot_longer(-age, names_to = "from_to", values_to = "p_conditioned")%>% 
  mutate(variant = "pij455")

# where did p_tibble_extrap come from?
p_tibble_extrapS <- 
  p_tibble_extrap %>% 
  mutate(HHS = HH / (1-HD),
         HUS = HU / (1-HD),
         UUS = UU / (1-UD),
         UHS = UH / (1-UD)) %>% 
  select(age, HHS, HUS, UUS, UHS) %>% 
  pivot_longer(-age, names_to = "from_to", values_to = "p_conditioned")%>% 
  mutate(variant = "old")
bind_rows(p455S,p_tibble_extrapS ) %>% 
  ggplot(aes(x=age,y=p_conditioned,color=from_to,linetype=variant)) +
  geom_line() +
  theme_minimal()
