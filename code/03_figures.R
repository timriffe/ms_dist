source("code/02_calculate.R")
optional_plots <- FALSE
# toggle this if manually executing the script, it'll generate
# several complementary plots that didn't make it into the manuscript.
# These may be diagnostics or unshowcased results.


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
f2
ggsave("fig2.svg", f2)

# -----------------------------------------
# Figure 3





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


variance_table |> 
  filter(measure == "ADL") |> 
  mutate(sdx = sqrt(vle),
         sdh = sqrt(vh),
         sdu = sqrt(vu)) |> 
  unlist()


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


