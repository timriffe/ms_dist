# Health and Retirement Study, (RAND HRS Longitudinal File 2018, release Jul 2022 (V2)) public use dataset. Produced and distributed by the University of Michigan with funding from the National Institute on Aging (grant number NIA U01AG009740). Ann Arbor, MI, (2023).

library(tidyverse)
library(haven)
library(janitor)
library(lubridate)
library(collapse)
library(tidyfast)
# imach_course_file <- "/home/tim/Desktop/HRSdata_0206.dta"
# file.exists(imach_course_file)
rand_file <- "randhrs1992_2018v2_STATA/randhrs1992_2018v2.dta"
file.exists(rand_file)
hrs_in <- read_stata(rand_file)
# hrs_in <- read_stata(imach_course_file)
# hrs_in$alive_06 %>% unique()


# compose_imach_date <- function(month, year){
#   month <- ifelse(is.na(month), 99, month)
#   year <- ifelse(is.na(year), 9999, year)
#   paste(month, year, sep = "/")
# }

compose_imach_rabdate <- function(date, flag){
  # date  <- as_date(sasdate, origin = as_date("1960-01-01"))
  month <- ifelse(flag == 1 | is.na(date), 99, month(date))
  year  <- ifelse(flag == 3 | is.na(date), 9999, year(date))
  paste(month, year, sep = "/")
}
compose_imach_raddate <- function(month, year){
  month <- ifelse(is.na(month),99,month)
  year  <- ifelse(is.na(year),9999,year)
  paste(month, year, sep = "/")
}
compose_imach_wmiddate <- function(date){
  # date  <- as_date(sasdate, origin = as_date("1960-01-01"))
  month <- ifelse(is.na(date),"99",month(date))
  year  <- ifelse(is.na(date),"9999",year(date))
  paste(month, year, sep = "/")
}

# NA disabled should be -1
hrs_in %>% select(contains("wtr_nh"))
# adl codes:
# -1 NA/missing
# otherwise 1 (yes) 2 (no)
# -1 also for iwstat = 9
# 3 for died (iwstat 5)
hrs_processed_long <-
  hrs_in %>% 
  # cut down columns, don't keep spouse info, although we could, but then what weight??
  select(id = hhidpn,
         sex = ragender,
        
         ends_with("wstat") & starts_with("r"),
         ends_with("wtcrnh") & starts_with("r"),
         #ends_with("wtresp") & starts_with("r"),
         #ends_with("wtr_nh") & starts_with("r"),
         ends_with("wmid") & starts_with("r"),
         #starts_with("inw"),
         bdate = rabdate,
         bdateflag = rabflag,
         radyear,
         radmonth,
         ends_with("adl5a") & starts_with("r"),
         ends_with("iadl5a") & starts_with("r"),
         -reiwmid) %>% 
  clean_names() %>% 
  slice(1:5000) %>% 
  # this males a full stack, very long!
  pivot_longer(-c(id, bdate, bdateflag,radyear,radmonth,sex), names_to = "wave_var", values_to = "value") %>% 
  # separate() too tough to make regex work on all variables, so we do it manually
  fmutate(
    wave_var = substr(wave_var, 2, nchar(wave_var)),
        wave = parse_number(wave_var),
         var = gsub('[0-9]+', '', wave_var)) %>% 
  select(-wave_var) %>% 
  # bring variables back to columns
  pivot_wider(names_from = var, values_from = value) %>% 
  rename(wt = wtcrnh,
         t1 = iwmid,
         iadl = iadla,
         adl = adla) %>% 
  # create dates, remove haven labels
  filter(iwstat %in% c(1,4,5,9)) %>%  # 1 responded, 2 alive no response
  fmutate(sex = labelled::remove_val_labels(sex),
         iwstat = labelled::remove_val_labels(iwstat),
         t1 = labelled::remove_val_labels(t1),
         wt = labelled::remove_val_labels(wt),
         adl = labelled::remove_val_labels(adl),
         iadl = labelled::remove_val_labels(iadl),
         
         # convert to dates
         bdate = as_date(bdate, origin = as_date("1960-01-01")),
         t1 = as_date(t1, origin = as_date("1960-01-01")),
         age = interval(bdate, t1) / years(1),
         # dates given in 3 ways ...
         t1 = compose_imach_wmiddate(t1),
         bdate = compose_imach_rabdate(date=bdate,flag=bdateflag),
         ddate =  compose_imach_raddate(radmonth,radyear)) %>% 
  select(-bdateflag, -radyear, -radmonth) %>% 
    
  # restrict universe  
  # wave 1 all missing for adl5 and iadl5
  filter(age >= 50 | is.na(age), 
         wt > 0 | iwstat %in% c(4,5,9),
         wave > 4,
         iwstat != 7) %>% 
  # code for padding weights Magda says 0 weights mean 0 so we discard instead  
  # we want to replace 0 weights with NAs in order to use fill()
  # mutate(wt = ifelse(wt == 0, NA, wt)) %>% 
  # group_by(id) %>% 
  # tidyr::fill(wt, .direction = "downup") %>% 
    
    
 

  # add row to end of each group to capture death event
  # group_modify(~ add_row(.x)) %>% 
  # # population birth and death dates for new row
  # tidyr::fill(bdate,ddate,wt,t1,sex, .direction = "down") %>% 
  # tidyr::fill(wt, .direction = "up") %>% 
  # ungroup() 
  # recode variables to reduced spaces
  fmutate(t1 = if_else(is.na(wave) & ddate > t1, ddate, t1),
         iadl = data.table::fcase(iadl == 0, 1,
                             iadl > 0, 2,
                             iwstat == 5, 3,
                             iwstat %in% c(4,9) | is.na(iadl), -1),
         adl = data.table::fcase(adl == 0, 1,
                            adl > 0, 2,
                            iwstat == 5, 3,
                            iwstat %in% c(4,9) | is.na(adl), -1),
         sex = sex - 1,
         wt = wt / 100000) %>% 

  group_by(id) %>% 
  mutate(adl_to = lead(adl),
         iadl_to = lead(iadl),
         t2 = lead(t1)) %>% 
    filter(!is.na(wt)) %>% 
  select(id,
         female = sex,
         bdate,
         ddate,
         age,
         wt,
         wave,
         iwstat,
         t1,
         adl_from = adl,
         iadl_from = iadl,
         t2,
         adl_to,
         iadl_to) 
 
hrs_processed_long


hrs_processed_long %>% 
  write_csv("hrs_processed_long.csv")
hrs_processed_long <- read_csv("hrs_processed_long.csv")
# samples for Magda 
hrs_processed_long %>% 
  ungroup() %>% 
  filter(between(wave,10,13),
         !is.na(t2)) %>% 
  mutate(age2 = age^2,
         age3 = age^3,
         age4 = age^4) %>% 
  select(wt, female, bdate, ddate, t1, adl_from, t2, adl_to) %>% 
  write.table("imach/hrs_wave1012.txt", col.names = FALSE, quote = FALSE)


here::here()
#model=1+age+age*age.
#model=1+age+V1+age*V1.

# do this in terminal instead.
# system2("/usr/local/bin/imach-0.99r42", "/home/tim/workspace/ms_dist/hrs_wave12_imach.txt",wait=FALSE)
read_imach_transitions <- function(path){
  # this is hackish. The lag between Age and the actual 
  # first row of data actually can vary depending on the specification...
  X           <- read_lines(path) 
  header_line <- grepl(X,pattern="Age")
  header_info <- X[grepl(X,pattern="Age")]
  header_info <- str_split(header_info, pattern = " ") %>% unlist()
  keepers     <- grepl(header_info,pattern = "Age") | grepl(header_info,pattern = "p")
  header_info <- header_info[ keepers ]
  header_info <- gsub(header_info, pattern = "-", replacement = "")
  N           <- length(X)
  data_lines  <- X[(which(header_line) + 1):N]
  data_linesL <- str_split(data_lines, pattern = " ")
  lapply(data_linesL, function(x, header_info){
    indices <- x != "" & !grepl(x, pattern = "\\(") & !grepl(x, pattern = "\\)") 
    x = x[indices] %>% as.numeric() %>% t() 
    colnames(x) = header_info
    as_tibble(x)
  }, header_info = header_info) %>% 
    bind_rows()
}

read_imach_transitions("imach/hrs_wave1012f_imach/PROB_rhrs_wave1012f_imach.txt") %>% 
  select(age = Age,
         HH = p11,
         HU = p12,
         HD = p13,
         UH = p21,
         UU = p22,
         UD = p23) %>% 
  pivot_longer(-age, names_to = "from_to", values_to= "p") %>% 
  ggplot(aes(x=age,y=p,color=from_to)) +
  geom_line() +
  ylim(0,1)
# colnames(pij) <- c("age","HH", "HU", "HD", "UH", "UU", "UD")

hrs_processed %>% 
  filter(wave == 5) %>% 
  write_csv("hrs_wave5.csv")

hrs_processed <- read_csv("hrs_ready.csv")
hrs_processed %>% 
  pull(iadl_to) %>% unique()

rm(hrs_in,hrs_processed)

hrs_trans <- read_csv("pijfemHRS.csv")
hrs_trans <- read_csv("pijfemHRS_iadl.csv")
hrs_trans <- read_csv("pijmaleHRS.csv")
hrs_trans %>% 
  rename(age = Age1,
         HH = p11,
         HU = p12,
         HD = p13,
         UH = p21,
         UU = p22,
         UD = p23) %>% 
  pivot_longer(-age, names_to = "from_to", values_to = "p") %>% 
  ggplot(aes(x=age, y = p, color = from_to)) +
  geom_line()
