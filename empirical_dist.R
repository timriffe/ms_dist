# ------------------------------------------------------------------------------ #

source("aux_functions.R")

adl_iadl <- read_csv("hrs_adl_iadl_all.csv", show_col_types= FALSE) |>
  mutate(race = "all", .before=2)
srh <- read_csv("foltyn_hrs_transitions.csv", show_col_types= FALSE) |>
  mutate(measure = "SRH", .before = 1)

hrs_all <- bind_rows(srh, adl_iadl)

# ensure not a leaky system
hrs_all <-
  hrs_all |>
  # positive col range ensures it'll work with or without sex, age, variant cols
  pivot_longer(-c(measure,sex,race,age), names_to = "from_to", values_to = "p") |>
  mutate(from = substr(from_to,1,1)) |> 
  group_by(measure, sex, race, age, from) |>
  mutate(p = p / sum(p)) |>
  ungroup() |>
  select(-from) |>
  pivot_wider(names_from = from_to, values_from = p)

hrs_all |>
  # positive col range ensures it'll work with or without sex, age, variant cols
  pivot_longer(c(HU,HD,HH,UH,UU,UD), names_to = "from_to", values_to = "p") |>
  ggplot(aes(x=age,y=p,color=from_to,linetype=sex))+
  geom_line() +
  facet_wrap(race~measure)

# eyeball major differences in e50;
# LE does not need to match between specifications:
# SRH came from different HRS year range; different state spaces
# also give different results
hrs_all |>
  group_by(measure, sex, race) |>
  do(calc_ex_simple(p_tibble=.data))
# New spells of H must start from U or initial age of H

# Calculate dxh for all subsets
d_out <-
  hrs_all |>
  group_by(measure, sex, race) |>
  do(calc_dxh(p_tibble=.data))



# variance checks
d_out |>
  group_by(measure, sex, race) |>
  mutate(le = sum(x * dxsc),
         mh = sum(h * dxsc),
         mu = sum(u * dxsc)) |>
  summarize(hle    = sum(lxsc[current_state == "H"]),
            ule    = sum(lxsc[current_state == "U"]),
            le     = sum(lxsc),
            vle    = sum((x - le) ^ 2 * dxsc),
            vh     = sum((h - mh) ^ 2 * dxsc),
            vu     = sum((u - mu) ^ 2 * dxsc),
            cov_hu = sum((h - mh) * (u - mu) * dxsc)) |>
  mutate(vle_check = vh + vu + 2 * cov_hu)



# mean distance variants 
d_out |>
  group_by(measure, sex, race) |>
  mutate(le = sum(x * dxsc),
         mh = sum(h * dxsc),
         mu = sum(u * dxsc),
         euc_dist = sqrt((mh - h)^2 + (mu - u) ^2),
         man_dist = abs(mh - h) + abs(mu - u),
         cheb_dist = pmax(abs(mh - h), abs(mu - u))) |> 
  summarize(man = sum(dxsc * man_dist),
            euc = sum(dxsc * euc_dist),
            cheb = sum(dxsc * cheb_dist))

#
d_out_summarized <- d_out |>
  group_by(measure, sex, race, h, u) |>
  summarize(dxsc = sum(dxsc), .groups = "drop") 

d_mode <- d_out_summarized |>
  group_by(measure, sex, race) |>
  filter(dxsc == max(dxsc))

# ------------------------------------------------------------------------------ #

d_out_summarizedi |>
  ggplot(aes(x = h,
             y = u,
             z = dxsc)) +
  metR::geom_contour_fill() +
  scale_fill_continuous_sequential("PuBuGn", guide = "none") +
  coord_equal(clip = 'off') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y      = element_text(margin = margin(r = -17)),
        axis.text.x      = element_text(margin = margin(t = -40))) +
  # custom Lexis-ish grid
  annotate(geom  ="segment",
           x     = seq(0, 60, by = 10),
           xend  = seq(0, 60, by = 10),
           y     = rep(0, 7),
           yend  = seq(61, 1, by = -10), col = "#50505050") +
  annotate(geom  ="segment",
           y     = seq(0, 60, by = 10),
           yend  = seq(0, 60, by = 10),
           x     = rep(0, 7),
           xend  = seq(61, 1, by = -10), col = "#50505050") +
  annotate(geom  ="segment",
           y     = rep(0,7),
           yend  = seq(0, 60, by = 10),
           x     = seq(0, 60, by = 10),
           xend  = rep(0, 7), col = "#50505050") +
  annotate(geom  = "text",
           x     = rep(0, 4),
           y     = seq(0, 60, by = 20) + 2, 
           label = seq(50, 110, by = 20),
           angle = -45,
           size  = 3) +
  annotate(geom  = "text",
           x     = 0,
           y     = 65,
           label = "age at death",
           angle = -45) +
  metR::geom_contour2(aes(label = after_stat(level)), color = "#00000050", size = 0.5) +
  labs(x = "healthy years (h)",
       y = "unhealthy years (u)") +
  geom_point(aes(x = HLE, 
                 y = ULE),
             color = "#f0941d") +
  # HLE arrow
  annotate(geom     = "segment", 
           x        = HLE, 
           xend     = HLE, 
           y        = 45, 
           yend     = 38,
           arrow    = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate(geom     = "text",
           x        = HLE, 
           y        = 48, 
           label    = paste0("HLE\n(", sprintf("%.2f", round(HLE, 2)),")")) + 
  annotate(geom     = "segment",
           x        = HLE,
           xend     = HLE,
           y        = ULE,
           yend     = 37,
           linetype = 2, 
           color    = "#FFFFFF50") +
  # ULE arrow
  annotate(geom     = "segment",
           x        = 45,
           xend     = 38,
           y        = ULE,
           yend     = ULE,
           arrow    = arrow(type = "closed", length = unit(0.02, "npc")))  +
  annotate(geom     = "text",
           y        = ULE, 
           x        = 48, 
           label    = paste0("ULE\n(", round(ULE, 2), ")")) + 
  annotate(geom     = "segment",
           y        = ULE,
           yend     = ULE,
           x        = HLE,
           xend     = 37,
           linetype = 2, 
           color    = "#FFFFFF50") +
  # LE arrow
  annotate(geom     = "segment",
           x        = LE + 2,
           xend     = LE - 5,
           y        = -2,
           yend     = 5,
         arrow      = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate(geom     = "text",
           y        = -4, 
           x        = LE + 2,
           label    = paste0("LE (", round(LE, 2), ")")) +
  annotate(geom     = "segment",
           x        = LE-6,
           xend     = HLE,
           y        = 6,
           yend     = ULE,
           linetype = 2, 
           color    = "#FFFFFF50") +
  # label the mean
  geom_curve(

    aes(x     = 40, 
        y     = 30, 
        xend  = HLE + 1, 
        yend  = ULE),
    arrow     = arrow(length = unit(0.02, "npc")),
    ncp       = 20,
    linewidth = .25,
    curvature = -.2,
    color = "#f0941d"
  ) +
  annotate(geom  = "text",
           x     = 44, 
           y     = 28, 
           label = paste0("Mean\n(", sprintf("%.2f", round(HLE, 2)), ",", round(ULE, 2), ")")) +
# label the mode
  geom_point(data  = d_mode,
             aes(x = h, 
                 y = u),
             color = "#ebedbb") +
  geom_curve(
    aes(x = 35, y = 40, xend = d_mode$h, yend = d_mode$u+1),
    arrow = arrow(length = unit(0.02, "npc")),
    ncp = 20,
    linewidth = .25,
    curvature = .2
  ) +
  annotate("text",x = 39, y = 41, label = paste0("Mode (integer for now)\n(",d_mode$h,", ",d_mode$u,")"))
main_plot + labs(title = "HRS w12 males adl")
# ggsave(main_plot,filename = "main_plot.svg", width = 6,height=7,units="in")


main_plot


dh <- d_out |> 
  group_by(h) |>
  summarize(dxsc = sum(dxsc))

dh %>% 
  ggplot(aes(x = h, 
             y = dxsc)) +
  geom_step() +
  geom_vline(xintercept = HLE, 
             color      = "red") +
  theme_minimal() 

du <- d_out |>
  group_by(u) |>
  summarize(dxsc = sum(dxsc))

du |>
  ggplot(aes(x = u,
             y = dxsc)) +
  geom_step() +
  geom_vline(xintercept = ULE, 
             color      = "red") +
  geom_vline(xintercept = 20.5,
               color    = "blue") +
  theme_minimal() 

d_out |>
  mutate(ph = h / x) |>
  group_by(x, ph) |>
  summarize(dxsc    = sum(dxsc),
            .groups = "drop") |>
  mutate(H = dxsc * ph,
         U = dxsc * (1 - ph)) |>
  select(x, H, U) |>
  pivot_longer(c(H, U), 
               names_to  = "state", 
               values_to = "deaths") |>
  ggplot(aes(x    = x,
             y    = deaths, 
             fill = state)) +
  geom_col(width = 1) +
  labs(title = "Deaths by age (-50) and fraction of life lived by state\n(experimental)")

d_out |>
  mutate(ph = h / x,
         ph = ph - ph %% .03 + .01) |>
  group_by(ph) |>
  summarize(dxsc    = sum(dxsc),
            .groups = "drop") 
  ggplot(aes(x = ph,
             y = dxsc)) +
  geom_line() +
  labs(x     = "fraction of life healthy",
       y     = "deaths",
       title = "deaths by fraction of life lived healthy")

d_out |>
  mutate(ph = h / x) |>
  group_by(x,ph) |>
  summarize(lxsc    = sum(lxsc),
            .groups = "drop") |>
  mutate(H = lxsc * ph,
         U = lxsc * (1 - ph)) |>
  select(x, H, U) |>
  pivot_longer(c(H, U), 
               names_to  = "state", 
               values_to = "lx") |>
  ggplot(aes(x    = x,
             y    = lx, 
             fill = state)) +
  geom_col(width  = 1) +
  labs(title = "l(x) by age (-50) and fraction of accumulated life lived by state\n(experimental)") +
  theme_minimal()

hu <- d_out |> 
  group_by(h, u) |>
  summarize(dxsc = sum(dxsc)) |>
  pivot_wider(names_from  = u, 
              values_from = dxsc) |>
  column_to_rownames("h") |>
  as.matrix()

hu[is.na(hu)] <- 0
h             <- rownames(hu) |> as.numeric() + 0.5
u             <- colnames(hu) |> as.numeric() + 0.5

h_out <- seq(10, 30, by = 0.01)
u_out <- seq(10, 40, by = 0.01)



