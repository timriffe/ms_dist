# ------------------------------------------------------------------------------ #
library(colorspace)
library(data.table)
library(tidyverse)
library(collapse) # for fsubset(), fast subsetting
library(tictoc)
source("aux_functions.R")

# empirical distribution of occupancy time, that is.
# The brute force approach won't be practical, as there
# are too many probabilities to derive. Each is solvable,
# but some can be reached in too many ways.
p_tibble_extrap <- read_csv("transitions_extrap.csv", show_col_types = FALSE)

p_tibble_extrap1 <- read_csv("transitions_Lorenti_alr.csv",
                            show_col_types = FALSE) |> 
  mutate(variant = "Lorenti ITA 2012 GALI (alr)", .before = 1)


p_tibble_extrap2 <- read_csv("transitions_extrap_hrs.csv",
                            show_col_types = FALSE) |> 
  mutate(variant = "Foltyn USA SRH (80+ alr)", .before = 1) |> 
  mutate(sex = "f", .after = variant)

p_tibble_extrap3 <- read_csv("transitions_lievre2003_annual.csv",
                            show_col_types = FALSE) |> 
mutate(variant = "Lievre 2003 Disability", .before = 1) 

p_tibble_extrap4 <- read_csv("/home/tim/workspace/ms_dist/pij455.csv",
                             show_col_types = FALSE) %>% 
  rename(age = Age1, HH = p11, HU = p12, HD = p13, UH = p21, UU = p22, UD = p23) |>
  mutate(sex = "m",.before=age) |> 
  mutate(variant = "Muszynska-Sp ITA 2017ish GALI", .before = sex)

p_tibble_extrap5 <- read_csv("pijfemHRS.csv") |>
  rename(age = Age1,
         HH = p11,
         HU = p12,
         HD = p13,
         UH = p21,
         UU = p22,
         UD = p23) %>% 
  mutate(sex = "f",.before=age) |> 
  mutate(variant = "Muszynska-Sp HRS adl", .before = sex)

p_tibble_extrap6 <- read_csv("pijfemHRS_iadl.csv") |>
  rename(age = Age1,
         HH = p11,
         HU = p12,
         HD = p13,
         UH = p21,
         UU = p22,
         UD = p23) %>% 
  mutate(sex = "f",.before=age) |> 
  mutate(variant = "Muszynska-Sp HRS iadl", .before = sex)

p_tibble_extrap6 <- read_csv("pijmaleHRS.csv") |>
  rename(age = Age1,
         HH = p11,
         HU = p12,
         HD = p13,
         UH = p21,
         UU = p22,
         UD = p23) %>% 
  mutate(sex = "f",.before=age) |> 
  mutate(variant = "Muszynska-Sp HRS adl", .before = sex)
# make long then stack
p1l <- p_tibble_extrap1 |> 
  pivot_longer(-c(age,sex),names_to = "from_to", values_to = "p") 
p2l <- p_tibble_extrap2 |>
  pivot_longer(-age,names_to = "from_to", values_to = "p") 
p3l <- p_tibble_extrap3 |> 
  pivot_longer(-c(sex,age),names_to = "from_to", values_to = "p") 
p4l <- p_tibble_extrap4 |> 
  pivot_longer(-age,names_to = "from_to", values_to = "p")

bind_rows(p1l,p2l,p3l,p4l) |> 
  ggplot(aes(x=age,y=p,color=from_to,linetype=sex)) +
  geom_line() +
  facet_wrap(~variant) +
  theme_minimal()

# p_tibble_extrap %>% 
#   pivot_longer(-1,names_to = "from_to", values_to = "p") %>% 
#   ggplot(aes(x=age,y=p,color = from_to)) +
#   geom_line()

# Magda
p_tibble_extrap <-
  p_tibble_extrap4 |> 
  filter(age <=110)

# Angelo
p_tibble_extrap <-
  p_tibble_extrap1|> 
  filter(sex=="m")

# Lievre
p_tibble_extrap <-
  p_tibble_extrap3 |> 
  filter(sex=="m")

# Folyn SRH HRS
p_tibble_extrap <-
  p_tibble_extrap2 

# Muszynska-Spielauer HRS ADL
p_tibble_extrap <-
  p_tibble_extrap5 |> 
  filter(age <=110)
p_tibble_extrap <-
  p_tibble_extrap6 |> 
  filter(age <=110)
p_tibble_extrap <-
  p_tibble_extrap7 |> 
  filter(age <=110)

# ensure not a leaky system
p_tibble_extrap <-
  p_tibble_extrap |>
  # positive col range ensures it'll work with or without sex, age, variant cols
  pivot_longer(c(HU,HD,HH,UH,UU,UD), names_to = "from_to", values_to = "p") |>
  mutate(from = substr(from_to,1,1)) |> 
  group_by(age, from) |>
  mutate(p = p / sum(p)) |>
  ungroup() |>
  select(-from) |>
  pivot_wider(names_from = from_to, values_from = p)


# step 1: count spells by starting age.
p_tibble_extrap |> nrow()

# 61 ages, now brute force impossible.
hh <- p_tibble_extrap$HH
uh <- p_tibble_extrap$UH
uu <- p_tibble_extrap$UU
hu <- p_tibble_extrap$HU

n    <- nrow(p_tibble_extrap)

lu   <- c(init["U"], rep(0, n))
lh   <- c(init["H"], rep(0, n))

# init <- c(H=.9, U = .1)
init <- p_tibble_extrap |> 
  filter(age == 50) |> 
  init_constant()

for (i in 1:n) {
  lu[i + 1] <- lu[i] * uu[i] + lh[i] * hu[i]
  lh[i + 1] <- lh[i] * hh[i] + lu[i] * uh[i]
}

# New spells of H must start from U or initial age of H
sum(lu) + sum(lh)

HLE <- sum(lh)
ULE <- sum(lu)
LE  <- HLE + ULE

# spell termination probabilities
hend <- c(1 - hh, 1) # 1 for closeout

# ------------------------ #
# re-considered. If we iterate up the ages, summing units
# into cumulative occupation time distributions, then I think we 
# have it.

# namely, the stock at a given age can be divided into bins iteratively.
# 0,1        init & 1st age
# 0,1,2      2nd age
# 0,1,2,3    3rd age
# ...
# each of these is a full distribution on reference state s.
# but the next iteration of the distribution only depends on current
# state s and age a. 
#

# plot(50:111,lu,ylim=c(0,1),type='l',col="red",ylab="lxs",xlab="age")
# lines(50:111,lh)


calculate.this <- FALSE
if (calculate.this){

d3 <- expand.grid(h = 0:61,
                  age = 50:111,
                  current_state = c("H","U"),
                  l = 0,
                  stringsAsFactors = FALSE) |>
  fsubset(h <= (age - 50)) |>
  fmutate(l = data.table::fcase(
    age == 50 & current_state == "H" , init["H"],
    age == 50 & current_state == "U" , init["U"],
    default = 0))


tic()
for (a in 50:110) {
  
  d3n         <- fsubset(d3, age == a)
  possible_ds <- unique(d3n$h) |> sort()
  dt          <- fsubset(p_tibble_extrap,age == a)
  
  HH <- dt$HH
  HU <- dt$HU
  UU <- dt$UU
  UH <- dt$UH
  
  for (d in possible_ds) {
    
    d3nd <- fsubset(d3n, h == d)
    lxhH <- fsubset(d3nd, current_state == "H")$l
    lxhU <- fsubset(d3nd, current_state == "U")$l
      
    d3 <- d3 |>
      fmutate(l = fcase(
        age == a + 1 & h == d + 1 & current_state == "H", l + lxhH * HH + lxhU * UH,
        age == a + 1 & h == d     & current_state == "U", l + lxhU * UU + lxhH * HU,
        rep_len(TRUE, length(l)), l))
  }
}

toc()

# sum checks
d_out <- p_tibble_extrap |>
  select(age, HD, UD) |>
  rename(H = HD, 
         U = UD) |>
  pivot_longer(c(H, U), 
               names_to  = "current_state", 
               values_to = "qx") |> 
  right_join(d3, by = c("age", "current_state"), multiple = "all") |>
  mutate(qx  = ifelse(age == 111, 1, qx),
         dxs = qx * l,
         x   = age - 50,
         u   = x - h) |> 
  select(current_state, age, x, h, u, lxsc = l, dxsc = dxs)

# 
d3 |> write_csv("d3.csv")
d_out |> write_csv("d_out.csv")
}


d3 <- read_csv("d3.csv")

d_out$dxsc |> sum()

# variance checks
d_out |>
  mutate(le = sum(x * dxsc),
         mh = sum(h * dxsc),
         mu = sum(u * dxsc)) |>
  summarize(vle    = sum((x - le) ^ 2 * dxsc),
            vh     = sum((h - mh) ^ 2 * dxsc),
            vu     = sum((u - mu) ^ 2 * dxsc),
            cov_hu = sum((h - mh) * (u - mu) * dxsc)) |>
  mutate(vle_check = vh + vu + 2 * cov_hu)



# mean distance variants 
d_out |>
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
  group_by(h, u) |>
  summarize(dxsc = sum(dxsc), .groups = "drop") 

d_mode <- d_out_summarized |>
  filter(dxsc == max(dxsc))

# ------------------------------------------------------------------------------ #
main_plot <- d_out_summarized |>
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



