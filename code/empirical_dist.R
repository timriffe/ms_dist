# ------------------------------------------------------------------------------ #

optional_plots <- FALSE

# adl_iadl <- read_csv("hrs_adl_iadl_all.csv", show_col_types= FALSE) |>
#   mutate(race = "all", .before=2)
# srh <- read_csv("foltyn_hrs_transitions.csv", show_col_types= FALSE) |>
#   mutate(measure = "SRH", .before = 1)
# hrs_all <- bind_rows(srh, adl_iadl)

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





# mortality ratio looks plausible
share_all |>
  filter(measure == "ADL") |> 
  mutate(ratio = UD / HD) |> 
  ggplot(aes(x=age, y = ratio)) +
  geom_line() +
  xlim(50,110) +
  theme_minimal()

d_out_share <-
  share_all |>
  group_by(measure, sex) |>
  do(calc_dxh(p_tibble=.data)) |> 
  ungroup() 

(expectancies <- 
    share_all |>
    group_by(measure, sex) |>
    do(calc_ex_simple(p_tibble=.data)))

# compare w HMD
library(HMDHFDplus)
flt <- readHMDweb("ITA","fltper_1x1", username = Sys.getenv("us"), password = Sys.getenv("pw"))

flt |> 
  filter(between(Year, 2015, 2017), Age == 50) |> 
  pull(ex) |> mean()
# hrs_all <- read_csv("imach/hrs_adl_iadl_sex_period_all.csv")

# eyeball major differences in e50;
# LE does not need to match between specifications:
# SRH came from different HRS year range; different state spaces
# also give different results
# (expectancies <- 
#     hrs_all |>
#   group_by(measure, sex,period) |>
#   do(calc_ex_simple(p_tibble=.data)))
# 
# expectancies |> 
#   ggplot(aes(x=period,y=LE,color = sex)) +
#   geom_line() +
#   facet_wrap(~measure)
# New spells of H must start from U or initial age of H

# Calculate dxh for all subsets
# d_out <-
#   hrs_all |>
#   group_by(measure, sex, period) |>
#   do(calc_dxh(p_tibble=.data)) |> 
#   ungroup() 
# 
# d_out_inaki <-
#   d_out |> 
#   ungroup() |> 
#   group_by(measure,sex,period,age,x,h,u) |> 
#   summarize(lsxc = sum(lxsc),
#          dsxc = sum(dxsc), .groups = "drop")
# 
# write_csv(d_out_inaki,"d_out_inaki.csv")

# library(ggridges)
# d_out_share |> 
#   filter(measure == "GALI") |> 
#   group_by(h,u) |> 
#   summarize(lxsc = sum(lxsc),
#             dxsc = sum(dxsc)) |> 
#   filter(u %% 5 == 0) |> 
#   ggplot(aes(x = h, height = dxsc, y = as.factor(u))) +
#   geom_ridgeline(scale=200, alpha = .3, fill = "#1133FF",col = "#1133FF") # +
 # metR::geom_contour2(aes(label = after_stat(level)), color = "#00000050", size = 0.5) 





d_out_share |> group_by(measure,sex) |> 
  summarize(check = sum(dxsc)) |> pull(check)

# variance checks
variance_table <-
d_out_share |>
  summarize(
    le = sum((x + .5) * dxsc),
    hle = sum((h + .5) * dxsc),
    ule = sum((u + .5) * dxsc),
    vle    = sum(((x+.5) - le) ^ 2 * dxsc),
    vh     = sum(((h+.5) - hle) ^ 2 * dxsc),
    vu     = sum(((u+.5) - ule) ^ 2 * dxsc),
    cov_hu = sum(((h+.5) - hle) * ((u+.5) - ule) * dxsc),
    .by = c(measure, sex)) |>
  mutate(vle_check = vh + vu + 2 * cov_hu) 
variance_table
library(xtable)
variance_table |> 
  filter(measure == "ADL") |> 
  mutate(sdx = sqrt(vle),
         sdh = sqrt(vh),
         sdu = sqrt(vu)) |> 
  unlist()
d_out_share |> 
  filter(sex == "female", measure == "ADL") |> 
  mutate(h = h - h %% 5) |> 
  group_by(age,h) |> 
  summarize(lxh = sum(lxsc), .groups = "drop") |>
  mutate(Cxh = lxh / sum(lxh)) |> 
  ggplot(aes(x=age,y=Cxh,fill=h))+
  geom_col(width=1) +
  scale_fill_continuous_sequential() +
  theme_minimal() +
  theme(text = element_text(size=20))

# mean distance variants 
d_out_share |>
  group_by(measure, sex) |>
  mutate(le = sum((x+.5) * dxsc),
         mh = sum((h+.5) * dxsc),
         mu = sum((u+.5) * dxsc),
         euc_dist = sqrt((mh - (h+.5))^2 + (mu - (u+.5)) ^2),
         man_dist = abs(mh - (h+.5)) + abs(mu - (u+.5)),
         cheb_dist = pmax(abs(mh - (h+.5)), abs(mu - (u+.5)))) |> 
  summarize(man = sum(dxsc * man_dist),
            euc = sum(dxsc * euc_dist),
            cheb = sum(dxsc * cheb_dist))

#
d_out_summarized <- d_out_share |>
  group_by(measure, sex, h, u) |>
  summarize(dxsc = sum(dxsc), .groups = "drop") 

# dout_fine <- expand.grid(h = seq(0,60,by=.1),
#                          u = seq(0,60,by=.1)) |> 
#   filter(h+u < 60)
# d_for_mode <-
#   d_out_summarized |> 
#   filter(measure == "ADL") %>% 
#   gam(log(dxsc) ~ te(h, u, k = c(20,20)), data = .,
#       method = 'ML') |> 
#   predict(newdata=dout_fine) |> 
#   as_tibble() |> 
#   rename(dxsc=value) |> 
#   bind_cols(dout_fine)

# d_for_mode |> 
#   ggplot(aes(x=h,y=u,fill=exp(dxsc)) )+
#   geom_tile()

# d_mode <- d_for_mode |>
#   filter(dxsc == max(dxsc))
# 
# d_for_mode |> 
#   group_by(h) |> 
#   summarize(dxsc = sum(exp(dxsc)),.groups = "drop") |> 
#   filter(dxsc==max(dxsc))
# 
# d_for_mode |> 
#   group_by(u) |> 
#   summarize(dxsc = sum(exp(dxsc)),.groups = "drop") |> 
#   filter(dxsc==max(dxsc))
# 
# d_for_mode |> 
#   mutate(x=h+u) |> 
#   group_by(x) |> 
#   summarize(dxsc = sum(exp(dxsc)),.groups = "drop") |> 
#   filter(dxsc==max(dxsc))

# ------------------------------------------------------------------------------ #
d_out_summarizedi <-
  d_out_summarized |> 
  filter(measure == "ADL",  sex == "female")

# d_modei <-
#   d_mode |> 
#   filter(measure == "GALI", sex == "female")

HLEi <- expectancies |> 
  filter(measure == "ADL", sex == "female")

p <-
d_out_summarizedi |>
  filter(h <= 60, u <= 60, (h+u) <= 60) |> 
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
        axis.text.x      = element_text(margin = margin(t = -40)),
        text = element_text(size=20)) +
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
           size  = 5) +
  annotate(geom  = "text",
           x     = 5,
           y     = 60,
           label = "age at death",
           angle = -45,
           size  = 5) +
  metR::geom_contour2(aes(label = after_stat(level)), color = "#00000050", size = 0.5) +
  labs(x = "healthy years (h)",
       y = "unhealthy years (u)") +
  geom_point(aes(x = HLEi$HLE, 
                 y = HLEi$ULE),
             color = "#f0941d",
             size=2) +
  # HLE arrow
  annotate(geom     = "segment", 
           x        = HLEi$HLE, 
           xend     = HLEi$HLE, 
           y        = 45, 
           yend     = 38,
           arrow    = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate(geom     = "text",
           x        = HLEi$HLE, 
           y        = 48, 
           label    = paste0("HLE\n(", sprintf("%.2f", round(HLEi$HLE, 2)),")"),
           size     = 5) + 
  annotate(geom     = "segment",
           x        = HLEi$HLE,
           xend     = HLEi$HLE,
           y        = HLEi$ULE,
           yend     = 37,
           linetype = 2, 
           color    = "#11111150") +
  # ULE arrow
  annotate(geom     = "segment",
           x        = 45,
           xend     = 38,
           y        = HLEi$ULE,
           yend     = HLEi$ULE,
           arrow    = arrow(type = "closed", length = unit(0.02, "npc")))  +
  annotate(geom     = "text",
           y        = HLEi$ULE, 
           x        = 48, 
           label    = paste0("ULE\n(", round(HLEi$ULE, 2), ")"),
           size     = 5) + 
  annotate(geom     = "segment",
           y        = HLEi$ULE,
           yend     = HLEi$ULE,
           x        = HLEi$HLE,
           xend     = 37,
           linetype = 2, 
           color    = "#11111150") +
  # LE arrow
  annotate(geom     = "segment",
           x        = HLEi$LE + 2,
           xend     = HLEi$LE - 5,
           y        = -2,
           yend     = 5,
         arrow      = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate(geom     = "text",
           y        = -3, 
           x        = HLEi$LE + 6,
           label    = paste0("LE (", round(HLEi$LE, 2), ")"),
           size =5) +
  annotate(geom     = "segment",
           x        = HLEi$LE-6,
           xend     = HLEi$HLE,
           y        = 6,
           yend     = HLEi$ULE,
           linetype = 2, 
           color    = "#11111150") +
  # label the mean
  geom_curve(

    aes(x     = 40, 
        y     = 30, 
        xend  = HLEi$HLE + 1, 
        yend  = HLEi$ULE),
    arrow     = arrow(length = unit(0.02, "npc")),
    ncp       = 20,
    linewidth = .3,
    curvature = -.2,
    color = "#f0941d"
  ) +
  annotate(geom  = "text",
           x     = 44, 
           y     = 32, 
           label = paste0("Mean\n(", sprintf("%.2f", round(HLEi$HLE, 2)), ",", round(HLEi$ULE, 2), ")"),
           size  = 5) 
p
  ggsave("share_adl_females.svg",p,width=7,height=7,units="in")
  
  d_out_summarizedi <- d_out_share |>
    group_by(measure, sex, h, u) |>
    summarize(dxsc = sum(dxsc), .groups = "drop") |> 
    filter(measure == "ADL", sex == "female")
  
  dhi <-
  d_out_summarizedi |> 
    group_by(h) |> 
    summarize(dh = sum(dxsc))
  dui <-
    d_out_summarizedi |> 
    group_by(u) |> 
    summarize(du = sum(dxsc))
  dxi<-
    d_out_summarizedi |> 
    mutate(x=h+u) |> 
    group_by(x) |> 
    summarize(dx = sum(dxsc))

  p1<-
  dhi |> 
    ggplot(aes(x=h,y=dh)) +
    geom_line() +
    theme_minimal() +
    theme(text = element_text(size=20))+
    geom_area(fill = "#aaCCaa")+
    ylim(0,.1)+
    labs(title="d(h)")
  p2<- 
  dui|> 
    ggplot(aes(x=u,y=du)) +
    geom_line()+
    theme_minimal() +
    theme(text = element_text(size=20)) +
    geom_area(fill = "#CCaaaa")+
    ylim(0,.1)+
    labs(title="d(u)")
  
  p3<-
  dxi|> 
    ggplot(aes(x=x,y=dx)) +
    geom_line()+
    theme_minimal() +
    theme(text = element_text(size=20)) +
    geom_area(fill = "#aaaaCC")+
    ylim(0,.1)+
    labs(title="d(x)")
  library(patchwork)
pout <- p1 | p2 | p3
ggsave("fig4.svg",pout,width=11,height=7)    

  # label the mode
  # geom_point(data  = d_modei,
  #            aes(x = h, 
  #                y = u),
  #            color = "#ebedbb") +
  # geom_curve(
  #   aes(x = 35, y = 40, xend = d_modei$h, yend = d_modei$u+1),
  #   arrow = arrow(length = unit(0.02, "npc")),
  #   ncp = 20,
  #   linewidth = .25,
  #   curvature = .2
  # ) +
  # annotate("text",x = 39, y = 41, 
  #          label = paste0("Mode (integer for now)\n(",d_modei$h,", ",d_modei$u,")"))
# main_plot + labs(title = "HRS w12 males adl")
# ggsave(main_plot,filename = "main_plot.svg", width = 6,height=7,units="in")


main_plot


dh <- d_out_share |> 
  group_by(h) |>
  summarize(dxsc = sum(dxsc))

dh %>% 
  ggplot(aes(x = h, 
             y = dxsc)) +
  geom_step() +
  geom_vline(xintercept = HLE, 
             color      = "red") +
  theme_minimal() 

du <- d_out_share |>
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

d_out_share |>
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

bin_size <- .04
d_out_share |>
  mutate(ph = h / x,
         ph = ph - ph %% bin_size + bin_size/2) |>
  group_by(ph) |>
  summarize(dxsc    = sum(dxsc),
            .groups = "drop") |> 
  ggplot(aes(x = ph,
             y = dxsc)) +
  geom_line() +
  labs(x     = "fraction of life healthy",
       y     = "deaths",
       title = "deaths by fraction of life lived healthy")

d_out_share |>
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



