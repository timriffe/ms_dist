# Health and Retirement Study, (RAND HRS Longitudinal File 2018, release Jul 2022 (V2)) public use dataset. Produced and distributed by the University of Michigan with funding from the National Institute on Aging (grant number NIA U01AG009740). Ann Arbor, MI, (2023).

library(tidyverse)
library(haven)
library(janitor)
rand_file <- "randhrs1992_2018v2_STATA/randhrs1992_2018v2.dta"
file.exists(rand_file)
hrs_in <- read_stata(rand_file)


cols_pick <- tibble(rand_name = c("HHIDPN"), 
                    
                    
                    new_name = c("id"))

"HHIDPN" # id
# wave response indicator (1,0)
starts_with("INW")
ends_with("WSTAT")  # respondent 1
                    # dead 5
ends_with("WTCRNH") # person weight (combined respondent and nursing home)
ends_with("WMID") # interview midpoint date
ends_with("BDATE") # birth date, missing imputed days since 1/1/1960
ends_with("DDATE") # death date, missings imputed; days since 1/1/1960
"RAGENDER" # gender; 1 male 2 female
ends_with("ADL5A") # count of 5 adls from wave 2 forward
ends_with("IADL5A") # count of 5 adls from wave 2 forward

dim(hrs_in)
colnames(hrs_in)[grepl(colnames(hrs_in),pattern="id")]
hrs_in2 <- 
hrs_in %>% 
  select(id = hhidpn,
         sex = ragender,
         ends_with("wstat") & starts_with("r"),
         ends_with("wtcrnh") & starts_with("r"),
         ends_with("wmid") & starts_with("r"),
         bdate = rabdate,
         ddate = raddate,
         ends_with("adl5a") & starts_with("r"),
         ends_with("iadl5a") & starts_with("r"),
         -reiwmid) %>% 
  clean_names()
hrs_in2 %>% colnames()
pas_test <- 
hrs_in2 %>% 
  # slice(1:5000) %>% 
  pivot_longer(-c(id, bdate, ddate,sex), names_to = "wave_var", values_to = "value") %>% 
 # pull(wave_var) %>% unique()
  mutate(
    wave_var = substr(wave_var, 2, nchar(wave_var)),
        wave = parse_number(wave_var),
         var = gsub('[0-9]+', '', wave_var)) %>% 
  select(-wave_var) %>% 
  pivot_wider(names_from = var, values_from = value) %>% 
  rename(wt = wtcrnh,
         t1 = iwmid,
         iadl = iadla,
         adl = adla) %>% 
  mutate(sex = labelled::remove_val_labels(sex),
         iwstat = labelled::remove_val_labels(iwstat),
         t1 = labelled::remove_val_labels(t1),
         wt = labelled::remove_val_labels(wt),
         adl = labelled::remove_val_labels(adl),
         iadl = labelled::remove_val_labels(iadl),
         t1 = as_date(t1, origin = as_date("1960-01-01")),
         bdate = as_date(bdate, origin = as_date("1960-01-01")),
         ddate =  as_date(ddate, origin = as_date("1960-01-01"))) %>% 
  filter(!is.na(t1)) %>% 
  mutate(wt = ifelse(wt == 0, NA, wt)) %>% 
  group_by(id) %>% 
  tidyr::fill(wt) %>% 
  filter(wave > 1) %>% 
  group_by(id) %>% 
  group_modify(~ add_row(.x)) %>% 
  tidyr::fill(bdate,ddate,wt,t1) %>% 
  ungroup() %>% 
  mutate(t1 = if_else(is.na(wave) & ddate > t1, ddate, t1),
         iadl_from = case_when(iadl == 0 ~ "H",
                          iadl > 0 ~ "U",
                          ddate == t1 ~ "D",
                          is.na(adl) & iwstat %in% c(4,9) ~ "A",
                          TRUE~NA_character_),
         adl_from = case_when(adl == 0 ~ "H",
                         adl > 0 ~ "U",
                         ddate == t1 ~ "D",
                         is.na(adl) & iwstat %in% c(4,9) ~ "A",
                         TRUE~NA_character_),
         sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female",
                         TRUE ~ NA_character_)) %>% 
  group_by(id) %>% 
  mutate(age = as.double(difftime(t1,bdate,units="days")/365),
         t1 = paste(month(t1),year(t1),sep="/"),
         bdate = paste(month(bdate),year(bdate),sep="/"),
         ddate = paste(month(ddate),year(ddate),sep="/"),
         ddate = if_else(ddate == "NA/NA", NA_character_, ddate),
         t2 = lead(t1),
         adl_to = lead(adl_from),
         iadl_to = lead(iadl_from)) %>% 
  ungroup() %>% 
  filter(!is.na(iwstat)) %>% 
  select(id,sex,bdate,ddate,wt,wave,iwstat,age,t1,t2,adl_from,adl_to,iadl_from,iadl_to)

pas_test %>% 
  write_csv("hrs_ready.csv")
pas_test %>% 
  filter(wave == 12) %>% 
  write_csv("hrs_wave12.csv")
  
pas_test %>% 
  filter(wave == 5) %>% 
  write_csv("hrs_wave5.csv")
