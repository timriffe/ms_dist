source("code/00_setup.R")
source("code/01_functions.R")


share_all <- 
  read_csv("data/share_all.csv",show_col_types = FALSE) |> 
  select(-version) |>
  # positive col range ensures it'll work with or without sex, age, variant cols
  pivot_longer(-c(country, sex, measure,age), names_to = "from_to", values_to = "p") |>
  mutate(from = substr(from_to,1,1),
         to = substr(from_to,2,2)) |> 
  # ensure not a leaky system 
  group_by(measure, sex, age, from) |>
  mutate(p = p / sum(p)) |>
  ungroup() |>
  mutate(p = case_when(age == max(age) & to == "D" ~ 1,
                       age == max(age) & to != "D" ~ 0,
                       TRUE ~ p)) |> 
  select(-from, -to) |>
  pivot_wider(names_from = from_to, values_from = p)


d_out_share <-
  share_all |>
  group_by(measure, sex) |>
  do(calc_dxh(p_tibble=.data)) |> 
  ungroup() 

d_out_summarized <- d_out_share |>
  group_by(measure, sex, h, u) |>
  summarize(dxsc = sum(dxsc), .groups = "drop") 

d_out_summarizedi <-
  d_out_summarized |> 
  filter(measure == "ADL",  sex == "female")

expectancies <- 
    share_all |>
    group_by(measure, sex) |>
    do(calc_ex_simple(p_tibble=.data))

HLEi <- expectancies |> 
  filter(measure == "ADL", sex == "female")
