
optional_plots <- FALSE
# toggle this if manually executing the script, it'll generate
# several complementary plots that didn't make it into the manuscript.
# These may be diagnostics or unshowcased results.
share_all <- 
  read_csv("share_all.csv",show_col_types = FALSE) |> 
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

# ----------------------------------------------- #
if (optional_plots){
  # View all transitions
  share_all |>
    # positive col range ensures it'll work with or without sex, age, variant cols
    pivot_longer(c(HU,HD,HH,UH,UU,UD), names_to = "from-to", values_to = "p") |>
    ggplot(aes(x=age,y=p,color=`from-to`,linetype=sex))+
    geom_line() +
    facet_wrap(~measure) +
    xlim(50,119) +
    theme_minimal()
  
  # Note: TR decided not to use GALI based on this plot:
  # mortality for people without GALI disability is higher
  # than those with, and it makes no sense to me, but we
  # didn't have resources to dig into it.
  
}

# Make Figure 2: (transitions for ADL)
f2 <-
  share_all |>
  filter(measure == "ADL") |> 
  # positive col range ensures it'll work with or without sex, age, variant cols
  pivot_longer(c(HU,HD,HH,UH,UU,UD), names_to = "from-to", values_to = "p") |>
  mutate(from = substr(`from-to`,1,1),
         to = substr(`from-to`,2,2)) |> 
  ggplot(aes(x=age,y=p,color=`from-to`,linetype=sex))+
  geom_line() +
  theme_minimal() +
  xlim(50,110) +
  theme(text = element_text(size=20)) +
  guides(linetype = "none") +
  facet_wrap(~from)

ggsave("fig2.svg", f2)

