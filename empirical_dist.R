
library(colorspace)
library(tidyverse)
library(tictoc)
library(collapse) # for fsubset(), fast subsetting
source("aux_functions.R")
# empirical distribution of occupancy time, that is.
# The brute force approach won't be practical, as there
# are too many probabilities to derive. Each is solvable,
# but some can be reached in too many ways.

p_tibble_extrap <- read_csv("transitions_extrap.csv",
                            show_col_types = FALSE)
# step 1: count spells by starting age.

p_tibble_extrap %>% nrow()
# 61 ages, now brute force impossible.


hh <- p_tibble_extrap[["HH"]]
uh <- p_tibble_extrap[["UH"]]
uu <- p_tibble_extrap[["UU"]]
hu <- p_tibble_extrap[["HU"]]

n <- nrow(p_tibble_extrap)

init <- c(H=.9, U = .1)
lu <- c(init["U"],rep(0,n))
lh <- c(init["H"],rep(0,n))
for (i in 1:n){
  lu[i+1] <- lu[i] * uu[i] + lh[i] * hu[i]
  lh[i+1] <- lh[i] * hh[i] + lu[i] * uh[i]
}
# New spells of H must start from U or initial age of H
sum(lu) + 
  sum(lh)

HLE <- sum(lh)
ULE <- sum(lu)
LE  <- HLE + ULE

# spell termination probabilities
hend <- c(1 - hh, 1) # 1 for closeout
# we have n+1 elements (up toa ge 111)
n    <- nrow(p_tibble_extrap) # only to age 110

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

ages <- p_tibble_extrap$age

calculate.this <- FALSE
if (calculate.this){
d3 <- expand.grid(h = 0:61,
                  age = 50:111,
                  current_state = c("H","U"),
                  l = 0,
                  stringsAsFactors = FALSE) %>% 
  dplyr::filter(h <= (age - 50)) %>% 
  mutate(l = case_when(age == 50 & current_state == "H" ~ .9,
                       age == 50 & current_state == "U" ~ .1,
                       TRUE ~ 0))

tic()
for (a in 50:110) {
  
  d3n              <- fsubset(d3, age == a)
  possible_ds      <- unique(d3n$h) |> sort()
  dt               <- fsubset(p_tibble_extrap,age == a)
  
  HH <- dt$HH
  HU <- dt$HU
  UU <- dt$UU
  UH <- dt$UH
  
  for (d in possible_ds) {
    
    d3nd <- fsubset(d3n, h == d)
    lxhH <- fsubset(d3nd, current_state == "H")$l
    lxhU <- fsubset(d3nd, current_state == "U")$l
    
    # those that increment health
    ind1 <- d3$age == a + 1 & d3$h == d + 1 & d3$current_state == "H"
    d3$l[ind1] <- d3$l[ind1] + lxhH * HH + lxhU * UH
    
    # those that increment health
    ind2 <- d3$age == a + 1 & d3$h == d & d3$current_state == "U"
    d3$l[ind2] <- d3$l[ind2] + lxhH * HH + lxhU * UH
  }
}
toc()
#toc()

# a tidy way to do the same, far slower due to subsetting. This was
# an early version, replaced with RTZ's base refactorization, further
# modified by TR for more speed.
# t1 <- tic()
# for (a in 50:110){
#   possible_ds <- d3 %>% 
#     filter(age == a) %>% 
#     pull(h) %>% 
#     unique()
#   
#   HH <- p_tibble_extrap %>% filter(age == a) %>% pull(HH)
#   HU <- p_tibble_extrap %>% filter(age == a) %>% pull(HU)
#   UU <- p_tibble_extrap %>% filter(age == a) %>% pull(UU)
#   UH <- p_tibble_extrap %>% filter(age == a) %>% pull(UH)
#   
#   
#   for (d in possible_ds){
#     
#     lxhH <-  d3 %>% 
#       filter(age == a,
#              h == d,
#              current_state == "H") %>% 
#       pull(l)
#     lxhU <- d3 %>% 
#       filter(age == a,
#              h == d,
#              current_state == "U") %>% 
#       pull(l)
#     
#     d3 <- d3 %>% 
#       mutate(l = case_when(# increment to current healthy in next time step
#         age == a + 1 & 
#           h == d + 1 & 
#           current_state == "H" ~ l + lxhH * HH + lxhU * UH,
#         # increment to current unhealthy in next time step
#         age == a + 1 & 
#           h == d & 
#           current_state == "U" ~ l + lxhU * UU + lxhH * HU,
#         TRUE ~ l))
#     
#   }
#   
# }
# t2 <- toc()
# sum checks
d3 %>% 
  filter(current_state == "H") %>% 
  pull(l) %>% sum()
sum(lh)

d3 %>% 
  filter(current_state == "U") %>% 
  pull(l) %>% sum()
sum(lu)

d3 %>% write_csv("d3.csv")
}


d3 <- read_csv("d3.csv")

d_out <- 
  p_tibble_extrap %>% 
  select(age, HD, UD) %>% 
  rename(H=HD,U=UD) %>% 
  pivot_longer(c(H,U), names_to = "current_state", values_to = "qx") %>% 
  right_join(d3,by=c("age","current_state")) %>% 
  mutate(qx = ifelse(age == 111,1,qx),
         dxs = qx * l,
         x = age - 50,
         u = x - h) %>% 
  select(current_state, age, x, h, u, lxsc = l, dxsc = dxs)

d_out$dxsc %>% sum()

d_out %>% 
  mutate(le = sum(x * dxsc),
         mh = sum(h * dxsc),
         mu = sum(u * dxsc)) %>% 
  summarize(vle = sum((x - le)^2 * dxsc),
            vh = sum((h - mh) ^ 2 * dxsc),
            vu = sum((u - mu) ^ 2 * dxsc),
            cov_hu = sum((h - mh) * (u - mu) * dxsc)) %>% 
  mutate(vle_check = vh + vu + 2 * cov_hu)



d_out %>% 
  mutate(le = sum(x * dxsc),
         mh = sum(h * dxsc),
         mu = sum(u * dxsc),
         euc_dist = sqrt((mh - h)^2 + (mu - u) ^2),
         man_dist = abs(mh - h) + abs(mu - u),
         cheb_dist = pmax(abs(mh - h), abs(mu - u))) %>% 
  summarize(man = sum(dxsc * man_dist),
            euc = sum(dxsc * euc_dist),
            cheb = sum(dxsc * cheb_dist))

#


d_out_summarized <-
  d_out %>% 
  group_by(h,u) %>% 
  summarize(dxsc = sum(dxsc), .groups = "drop") 

d_mode <-
  d_out_summarized %>% 
  filter(dxsc == max(dxsc))

main_plot <-
d_out_summarized %>% 
  ggplot(aes(x=h,y=u,z = dxsc)) +
  #geom_contour_filled() +
  #geom_contour() +
  metR::geom_contour_fill() +
  scale_fill_continuous_sequential("PuBuGn",guide="none") +
  coord_equal(clip = 'off') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(margin = margin(r = -17)),
        axis.text.x = element_text(margin = margin(t = -40))) +
  # custom Lexis-ish grid
  annotate("segment",
           x=seq(0,60,by=10),
           xend = seq(0,60,by=10),
           y=rep(0,7),
           yend=seq(61,1,by=-10), col = "#50505050") +
  annotate("segment",
           y=seq(0,60,by=10),
           yend = seq(0,60,by=10),
           x=rep(0,7),
           xend=seq(61,1,by=-10), col = "#50505050") +
  annotate("segment",
           y=rep(0,7),
           yend = seq(0,60,by=10),
           x=seq(0,60,by=10),
           xend=rep(0,7), col = "#50505050") +
  annotate(geom = "text",
           x=rep(0, 4),
           y=seq(0, 60, by = 20)+2, 
           label = seq(50, 110, by = 20),
           angle=-45,
           size = 3) +
  annotate(geom = "text",
           x=0,
           y=65,
           label = "age at death",
           angle=-45) +
  # annotate(geom = "segment", 
  #          x= 20,
  #          y=47,
  #          xend=18,
  #          yend=45,arrow=arrow(type="closed", length = unit(0.02,"npc"))) +
  metR::geom_contour2(aes(label = after_stat(level)), color = "#00000050", size = .5) +
  labs(x = "healthy years (h)",
       y = "unhealthy years (u)") +
  geom_point(aes(x=HLE, y = ULE),
             color = "#f0941d") +
  # HLE arrow
  annotate("segment",x=HLE,xend=HLE,y=45,yend=38,
           arrow=arrow(type="closed", length = unit(0.02,"npc"))) +
  annotate("text",x=HLE, y = 48, label = paste0("HLE\n(",sprintf("%.2f",round(HLE,2)),")")) + 
  annotate("segment",x=HLE,xend=HLE,y=ULE,yend=37,linetype=2, color = "#FFFFFF50") +
  # ULE arrow
  annotate("segment",x=45,xend=38,y=ULE,yend=ULE,
           arrow=arrow(type="closed", length = unit(0.02,"npc")))  +
  annotate("text",y=ULE, x = 48, label = paste0("ULE\n(",round(ULE,2),")")) + 
  annotate("segment",y=ULE,yend=ULE,x=HLE,xend=37,linetype=2, color = "#FFFFFF50") +
  # LE arrow
  annotate("segment",x=LE+2,xend=LE-5,y=-2,yend=5,
         arrow=arrow(type="closed", length = unit(0.02,"npc"))) +
  annotate("text",y=-4, x = LE+2, label = paste0("LE (",round(LE,2),")")) +
  annotate("segment",x=LE-6,xend=HLE,y=6,yend=ULE,linetype=2, color = "#FFFFFF50") +
  # label the mean
  geom_curve(
    aes(x = 40, y = 30, xend = HLE+1, yend = ULE),
    arrow = arrow(length = unit(0.02, "npc")),
    ncp = 20,
    linewidth = .25,
    curvature = -.2
  ) +
  annotate("text",x = 44, y = 28, label = paste0("Mean\n(",sprintf("%.2f",round(HLE,2)),",",round(ULE,2),")")) +
# label the mode
  geom_point(data = d_mode,
             aes(x=h,y=u),
             color = "#ebedbb") +
  geom_curve(
    aes(x = 35, y = 40, xend = 19, yend = 22),
    arrow = arrow(length = unit(0.02, "npc")),
    ncp = 20,
    linewidth = .25,
    curvature = .2
  ) +
  annotate("text",x = 39, y = 41, label = paste0("Mode (integer for now)\n(19,21)"))
main_plot
ggsave(main_plot,filename = "main_plot.svg", width = 6,height=7,units="in")

dh <-
  d_out %>% 
  group_by(h) %>% 
  summarize(dxsc = sum(dxsc))
dh %>% 
  ggplot(aes(x=h,y=dxsc)) +
  geom_step() +
  geom_vline(xintercept = HLE, color = "red") +
  theme_minimal() 
du <-
  d_out %>% 
  group_by(u) %>% 
  summarize(dxsc = sum(dxsc))
du %>% 
  ggplot(aes(x=u,y=dxsc)) +
  geom_step() +
  geom_vline(xintercept = ULE, color = "red") +
  geom_vline(xintercept = 20.5,
               color = "blue") +
  theme_minimal() 

d_out %>% 
  mutate(ph = h / x) %>% 
  group_by(x,ph) %>% 
  summarize(dxsc = sum(dxsc),
            .groups = "drop") %>% 
  mutate(H = dxsc * ph,
         U = dxsc * (1 - ph)) %>% 
  select(x, H, U) %>% 
  pivot_longer(c(H, U), names_to = "state", values_to = "deaths") %>% 
  ggplot(aes(x=x,y=deaths, fill = state)) +
  geom_col(width = 1) +
  labs(title = "Deaths by age (-50) and fraction of life lived by state\n(experimental)")

d_out %>% 
  mutate(ph = h / x,
         ph = ph - ph %% .03 + .01) %>% 
  group_by(ph) %>% 
  summarize(dxsc = sum(dxsc),
            .groups = "drop") %>% 
  ggplot(aes(x=ph,y=dxsc)) +
  geom_line() +
  labs(x = "fraction of life healthy",
       y="deaths",
       title = "deaths by fraction of life lived healthy")

d_out %>% 
  mutate(ph = h / x) %>% 
  group_by(x,ph) %>% 
  summarize(lxsc = sum(lxsc),
            .groups = "drop") %>% 
  mutate(H = lxsc * ph,
         U = lxsc * (1 - ph)) %>% 
  select(x, H, U) %>% 
  pivot_longer(c(H, U), names_to = "state", values_to = "lx") %>% 
  ggplot(aes(x=x,y=lx, fill = state)) +
  geom_col(width = 1) +
  labs(title = "l(x) by age (-50) and fraction of accumulated life lived by state\n(experimental)") +
  theme_minimal()

hu <-
  d_out %>% 
  group_by(h,u) %>% 
  summarize(dxsc = sum(dxsc)) %>% 
  pivot_wider(names_from = u, values_from = dxsc) %>% 
  column_to_rownames("h") %>% 
  as.matrix()
hu[is.na(hu)] <- 0
h <- rownames(hu) %>% 
  as.numeric() + .5
u <- colnames(hu) %>% as.numeric() + .5

h_out <- seq(10,30,by=.01)
u_out <- seq(10,40,by=.01)





do.old <- FALSE
if (do.old){
# Again, collecting both H and U distributions.
d2 <- expand.grid(dur=0:61,
                  age=50:111,
                  ref_state = c("H","U"),
                  current_state=c("H","U"),
                  f=0) %>% 
  filter(dur <= (age-50))%>% 
  mutate(f = case_when(ref_state == "H" & age == 50 & dur == 0 & current_state == "H" ~ .8,
                       ref_state == "H" & age == 50 & dur == 0 & current_state == "U" ~ .2,
                       ref_state == "U" & age == 50 & dur == 0 & current_state == "H" ~ .8,
                       ref_state == "U"& age == 50 & dur == 0 & current_state == "U" ~ .2,
                       TRUE ~ 0))
d2 %>% filter(age == 51)
for (a in 50:110){
  possible_ds <- d2 %>% filter(age == a) %>% pull(dur) %>% unique() %>% sort()
  
  HH <- p_tibble_extrap %>% filter(age == a) %>% pull(HH)
  HU <- p_tibble_extrap %>% filter(age == a) %>% pull(HU)
  UU <- p_tibble_extrap %>% filter(age == a) %>% pull(UU)
  UH <- p_tibble_extrap %>% filter(age == a) %>% pull(UH)

  
  for (d in possible_ds){
    Hhad <-
      d2 %>% 
      filter(age == a, dur == d, current_state == "H", ref_state== "H" ) %>% 
      pull(f)
    
    Huad <-
      d2 %>% 
      filter(age == a, dur == d, current_state == "U", ref_state== "H") %>% 
      pull(f)
    Uhad <-
      d2 %>% 
      filter(age == a, dur == d, current_state == "H", ref_state== "U" ) %>% 
      pull(f)
    
    Uuad <-
      d2 %>% 
      filter(age == a, dur == d, current_state == "U", ref_state== "U") %>% 
      pull(f)
    
    d2 <- d2 %>% 
      mutate(f = case_when(ref_state == "H" & 
                             age == a + 1 & 
                             dur == d + 1 & 
                             current_state == "H" ~ f + Hhad * HH + Huad * UH,
                           
                           ref_state == "H" & 
                             age == a + 1 & 
                             dur == d & 
                             current_state == "U" ~ f + Huad * UU + Hhad * HU,
                           
                           ref_state == "U" & 
                             age == a + 1 & 
                             dur == d & 
                             current_state == "H" ~ f + Uuad * UH + Uhad * HH,
                           
                           ref_state == "U" & 
                             age == a + 1 & 
                             dur == d + 1 & 
                             current_state == "U" ~ f + Uuad * UU + Uhad * HU,
                           
                           TRUE ~ f))
      
}
}

# sums check
d2 %>% 
  group_by(ref_state) %>% 
  summarize(e0 = sum(f)) %>% 
  pull(e0)

sum(lu)+sum(lh) # yay

qx <-
p_tibble_extrap %>% 
  select(age,HD,UD) %>% 
  bind_rows(tibble(age=111,HD=1,UD=1))

d2_save <-
d2 %>% 
  left_join(qx,by="age") %>% 
  mutate(deaths = ifelse(current_state == "H", f * HD, f * UD)) %>% 
  select(-HD,-UD) 

write_csv(d2_save, file = "d2.csv")

# note, we can get du from age and dh and 
# we can get dg from age and du
d2 <- read_csv("d2.csv",
               show_col_types = FALSE) %>% 
  filter(ref_state == "H") %>% 
  rename(dh = dur) %>% 
  mutate(du = age - dh - 50) %>% 
  select(-ref_state)
d2



d2 %>% 
  group_by(dh,du) %>% 
  summarize(f = sum(deaths),.groups = "drop") %>% 
  ggplot(aes(x=dh,y=factor(du),height=f)) +
  ggridges::geom_ridgeline(scale=1000,alpha=.5,fill = "#d4a31c",color=gray(.5)) +
  theme_minimal() + 
  labs(y = "du") +
  coord_equal() +
  #geom_abline(intercept=0,slope=1) +
  geom_abline(intercept=seq(10,60,by=10),slope=-1,color = gray(.4),size=.5) +
  annotate(geom = "text",
           x=rep(0, 6),
           y=seq(10, 60, by = 10)-2, 
           label = seq(60, 110, by = 10),
           angle=-45) +
  annotate(geom = "text",
           x=20,
           y=50,
           label = "age at death",
           angle=-45) +
  annotate(geom = "segment", 
           x= 20,
           y=47,
           xend=18,
           yend=45,arrow=arrow(type="closed", length = unit(0.02,"npc")))

# weighted means match
d2 %>% 
  mutate(a = age - 50) %>% 
  summarize(am = sum(a * deaths) / sum(deaths),
            dum = sum(du * deaths) / sum(deaths),
            dhm = sum(dh * deaths) / sum(deaths),
            va = sum((a - am)^2 * deaths) / sum(deaths),
            vdh = sum((dh - dhm)^2 * deaths) / sum(deaths),
            vdu = sum((du - dum)^2 * deaths) / sum(deaths),
            # Euclidean distance
            ve = sum(((dh - dhm)^2+(du - dum)^2) * deaths) / sum(deaths)) %>% 
  mutate(v_check2 = vdh + vdu)

x <- c(  107,  26.7,  69.9)
powmean <- function(x,power){
 
  mean(x^power)^(1/power)
}
powmean(x,10)
d2 %>% 
  summarize(f = sum(deaths), .by = c(dh)) %>% 
  ggplot(aes(x=dh,y=f)) +
  geom_line()

pow <- 1
d2 %>% 
  mutate(le = sum((age-50) * deaths),
         mh = sum(dh * deaths),
         mu = sum(du * deaths),
         euc_dist = sqrt((mh - dh)^2 + (mu - du) ^2),
         man_dist = abs(mh - dh) + abs(mu - du),
         cheb_dist = pmax(abs(mh - dh), abs(mu - du))) %>% 
  summarize(man = sum(deaths * man_dist),
            euc = sum(deaths * euc_dist),
            cheb = sum(deaths * cheb_dist))

library(philentropy)
distance_wrap <- function(mh,mu,dh,du, method){
  mat <- rbind(c(mh,mu),c(dh,du))
  distance(mat, method = method, mute.message = TRUE) 
}
d2 %>% 
  mutate(le = sum((age-50) * deaths),
         mh = sum(dh * deaths),
         mu = sum(du * deaths)) %>% 
  rowwise() %>% 
  mutate(euclidean = distance_wrap(mh,mu,dh,du, method = "euclidean"),
         chebyshev = distance_wrap(mh,mu,dh,du, method = "chebyshev"),
         harmonic_mean = distance_wrap(mh,mu,dh,du, method = "harmonic_mean"),
         manhattan = distance_wrap(mh,mu,dh,du, method = "manhattan"),
         avg = distance_wrap(mh,mu,dh,du, method = "avg")) %>% 
  ungroup() %>% 
  summarize(euclidean = sqrt(sum(euclidean^2*deaths)),
            chebyshev = sqrt(sum(chebyshev^2*deaths)),
            harmonic_mean = sqrt(sum(harmonic_mean*deaths)),
            manhattan = sqrt(sum(manhattan^2*deaths)),
            avg = sqrt(sum(avg^2*deaths)),
            vle = sqrt(sum(((age-50) - le)^2 * deaths))

            )
# D^2(X+Y)=D^2(X)+D^2(Y) + 2cov(X,Y)
# cov(X,Y) = E(XY)-EX \ EY

d2

d2 %>% 
  mutate(le = sum((age-50) * deaths),
         mh = sum(dh * deaths),
         mu = sum(du * deaths)) %>% 
  summarize(vle = sum(((age-50) - le)^2 * deaths),
            vh = sum((dh -mh) ^ 2 * deaths),
            vu = sum((du -mu) ^ 2 * deaths),
            cov_hu = sum((dh - mh) * (du - mu) * deaths)) %>% 
  mutate(vle_check = vh + vu + 2 * cov_hu)

d2 %>% 
  group_by(dh,du) %>% 
  summarize(f = sum(deaths), .groups = "drop") %>% 
  ggplot(aes(x=du,y=factor(dh),height=f)) +
  ggridges::geom_ridgeline(scale=1000,alpha=.5,fill = "#d4a31c",color=gray(.5)) +
  theme_minimal() + 
  labs(y = "dh") +
  coord_equal() +
  #geom_abline(intercept=0,slope=1) +
  geom_abline(intercept=seq(10,60,by=10),slope=-1,color = gray(.4),size=.5) +
  annotate(geom = "text",
           x=rep(0, 6),
           y=seq(10, 60, by = 10)-2, 
           label = seq(60, 110, by = 10),
           angle=-45) +
  annotate(geom = "text",
           x=20,
           y=50,
           label = "age at death",
           angle=-45) +
  annotate(geom = "segment", 
           x= 20,
           y=47,
           xend=18,
           yend=45,arrow=arrow(type="closed", length = unit(0.02,"npc")))

  
d2 %>% 
  group_by(age,du) %>% 
  summarize(f = sum(deaths), .groups = "drop") %>% 
  ggplot(aes(x=age,y=factor(du),height=f)) +
  ggridges::geom_ridgeline(scale=1000,alpha=.5,fill = "#d4a31c",color=gray(.5)) +
  theme_minimal() + 
  labs(y = "du") +
  coord_equal() +
  xlim(50,110) +
  #geom_abline(intercept=0,slope=1) +
  geom_abline(intercept=-seq(50,110,by=10),slope=1,color = gray(.4),size=.5) +
  annotate(geom = "text",
           x=rep(110, 6),
           y=seq(10, 60, by = 10)-2, 
           label = seq(50, 0, by = -10),
           angle=45) +
  annotate(geom = "text",
           x = 110,
           y = 62,
           label = "dh",
           angle = 45)

d2 %>% 
  group_by(age) %>% 
  summarize(f = sum(f)) %>% 
  ggplot(aes(x=age,y=f))+
  geom_line()




# TODO a TTD perspective...
# 
# d3 <- expand.grid(h = 0:61,
#                   age = 50:111,
#                   current_state = c("H","U"),
#                   l = 0,
#                   stringsAsFactors = FALSE) %>% 
#   dplyr::filter(h <= (age - 50)) %>% 
#   mutate(l = case_when(age == 50 & current_state == "H" ~ .9,
#                        age == 50 & current_state == "U" ~ .1,
#                        TRUE ~ 0))
# 
# 
# 
# for (a in 50:110){
#   possible_ds <- d3 %>% 
#     filter(age == a) %>% 
#     pull(h) %>% 
#     unique()
# 
#   HH <- p_tibble_extrap %>% filter(age == a) %>% pull(HH)
#   HU <- p_tibble_extrap %>% filter(age == a) %>% pull(HU)
#   UU <- p_tibble_extrap %>% filter(age == a) %>% pull(UU)
#   UH <- p_tibble_extrap %>% filter(age == a) %>% pull(UH)
#   
#   
#   for (d in possible_ds){
#     
#     lxhH <-  d3 %>% 
#       filter(age == a,
#              h == d,
#              current_state == "H") %>% 
#       pull(l)
#     lxhU <- d3 %>% 
#       filter(age == a,
#              h == d,
#              current_state == "U") %>% 
#       pull(l)
#     
#     d3 <- d3 %>% 
#       mutate(l = case_when(# increment to current healthy in next time step
#                            age == a + 1 & 
#                            h == d + 1 & 
#                            current_state == "H" ~ l + lxhH * HH + lxhU * UH,
#                            # increment to current unhealthy in next time step
#                            age == a + 1 & 
#                            h == d & 
#                            current_state == "U" ~ l + lxhU * UU + lxhH * HU,
#                            TRUE ~ l))
#     
#   }
# }
# 
# 
# # sum checks
# d3 %>% 
#   filter(current_state == "H") %>% 
#   pull(l) %>% sum()
# sum(lh)
# 
# d3 %>% 
#   filter(current_state == "U") %>% 
#   pull(l) %>% sum()
# sum(lu)
# 
# 
# d_out <- 
# p_tibble_extrap %>% 
#   select(age, HD, UD) %>% 
#   rename(H=HD,U=UD) %>% 
#   pivot_longer(c(H,U), names_to = "current_state", values_to = "qx") %>% 
#   right_join(d3,by=c("age","current_state")) %>% 
#   mutate(qx = ifelse(age == 111,1,qx),
#         dxs = qx * l,
#         x = age - 50,
#         u = x - h) %>% 
#   select(current_state, age, x, h, u, lxsc = l, dxsc = dxs)
# 
# d_out$dxs %>% sum()
# 
# d_out %>% 
#   mutate(le = sum(x * dxsc),
#          mh = sum(h * dxsc),
#          mu = sum(u * dxsc)) %>% 
#   summarize(vle = sum((x - le)^2 * dxsc),
#             vh = sum((h - mh) ^ 2 * dxsc),
#             vu = sum((u - mu) ^ 2 * dxsc),
#             cov_hu = sum((h - mh) * (u - mu) * dxsc)) %>% 
#   mutate(vle_check = vh + vu + 2 * cov_hu)
# d_out %>% 
#   mutate(le = sum(x * dxsc),
#          mh = sum(h * dxsc),
#          mu = sum(u * dxsc),
#          euc_dist = sqrt((mh - h)^2 + (mu - u) ^2),
#          man_dist = abs(mh - h) + abs(mu - u),
#          cheb_dist = pmax(abs(mh - h), abs(mu - u))) %>% 
#   summarize(man = sum(dxsc * man_dist),
#             euc = sum(dxsc * euc_dist),
#             cheb = sum(dxsc * cheb_dist))
# hu <-
# d_out %>% 
#   group_by(h,u) %>% 
#   summarize(dxsc = sum(dxsc)) %>% 
#   pivot_wider(names_from = u, values_from = dxsc) %>% 
#   column_to_rownames("h") %>% 
#   as.matrix()
# FX <- 
# d_out %>% 
#   group_by(x) %>% 
#   summarize(dxsc = sum(dxsc)) %>% 
#   column_to_rownames("x") %>% 
#   as.matrix()
# library(DescTools)
# 
# h <- rownames(hu) %>% as.integer()
# u <- colnames(hu) %>% as.integer()
# x <- rownames(FX) %>% as.integer()
# fh <- rowSums(hu, na.rm = TRUE)
# fu <- colSums(hu, na.rm = TRUE)
# fx <- rowSums(FX)
# Gini(h,fh, unbiased = FALSE)
# Gini(u,fu, unbiased = FALSE)
# Gini(x,fx, unbiased = FALSE)
# d2 %>% 
#   group_by(dur, ref_state) %>% 
#   summarize(f = sum(f),
#             .groups = "drop") %>% 
#   group_by(ref_state) %>% 
#   mutate(f = f / sum(f)) %>% 
#   ggplot(aes(x=dur,y=f,color = ref_state)) +
#   geom_line() + 
#   labs(title = "state occupancy distributions",
#        subtitle = "not at all surprising: I think a lot of short \nU spells are just prior to death.\nThat can also be worked out") +
#   theme_minimal()
# 
# 
# 
# d2 %>% 
#   group_by(age,dur,ref_state) %>% 
#   summarize(f=sum(f)) %>% 
#   ggplot(aes(x = age, y = f, color = dur, group = dur)) +
#   geom_line() +
#   facet_wrap(~ref_state)
# 
# d2 %>% 
#   group_by(age,dur,ref_state) %>% 
#   summarize(f=sum(f)) %>% 
#   ggplot(aes(x = dur, y = f, color = age, group = age)) +
#   geom_line() +
#   facet_wrap(~ref_state)
# 
# # can we back out a proper dist from d2?
# d2 %>% 
#   group_by(ref_state, age, dur) %>% 
#   summarize(f=sum(f), .groups = "drop") %>% 
#   arrange(ref_state,dur, age) %>% 
#   group_by(ref_state,dur) %>% 
# 
#   # head()
#   # summarize(f = sum(ff, na.rm =TRUE),.groups="drop") %>% 
#   ggplot(aes(x=dur,y=test,color=ref_state,group=ref_state)) +
#   geom_line()
# 
# 
# d2 %>% 
#   select(-deaths) %>% 
#   left_join(p_tibble_extrap %>% select(age,HD,UD),by="age") %>% 
#   mutate(deaths = ifelse(current_state == "H", f * HD, f * UD)) %>% 
#   group_by(ref_state,age) %>% 
#   summarize(deaths = sum(deaths),.groups= "drop") %>% 
#   ggplot(aes(x=age,y=deaths,color=ref_state)) +
#   geom_line() +
#   labs(title = "this looks more like what we expect to see")
# 
# d2 %>% 
#   left_join(p_tibble_extrap %>% 
#               select(age, HD, UD),
#             by = "age") %>% 
#   mutate(deaths = ifelse(current_state == "H", f * HD, f * UD)) %>% 
#   group_by(ref_state,dur) %>% 
#   summarize(deaths = sum(deaths, na.rm=TRUE),.groups= "drop") %>% 
#   ggplot(aes(x=dur,y=deaths,color=ref_state)) +
#   geom_line()
# 
# 
# d2 %>% 
#   left_join(p_tibble_extrap %>% 
#               select(age, HD, UD),
#             by = "age") %>% 
#   mutate(deaths = ifelse(current_state == "H", f * HD, f * UD)) %>% 
#   group_by(ref_state,dur,age) %>% 
#   summarize(deaths = sum(deaths, na.rm=TRUE),.groups= "drop") %>% 
#   filter(dur %% 5 == 0) %>% 
#   ggplot(aes(x=age,y=factor(dur),height=deaths,fill=ref_state)) +
#   ggridges::geom_ridgeline(scale=300,alpha=.5) +
#   labs(title = "age-at death distribution conditional on total lifetime state occupancy",
#        subtitle = "(this is a cross-section view on a single bivariate distribution)",
#        y="duration") +
#   theme_minimal()
# 
# d2 %>% 
#   left_join(p_tibble_extrap %>% 
#               select(age, HD, UD),
#             by = "age") %>% 
#   mutate(deaths = ifelse(current_state == "H", f * HD, f * UD)) %>% 
#   group_by(ref_state,dur,age) %>% 
#   summarize(deaths = sum(deaths, na.rm=TRUE),.groups= "drop") %>% 
#   filter(dur %% 5 == 0) %>% 
#   group_by(ref_state,dur) %>% 
#   mutate(deaths = deaths / sum(deaths)) %>% 
#   ggplot(aes(x=age,y=factor(dur),height=deaths,fill=ref_state)) +
#   ggridges::geom_ridgeline(scale=20,alpha=.5) +
#   labs(title = "each age-at death distribution\nconditioned on total time spent in each reference state",
#        y="duration") +
#   theme_minimal()
# 
# d2 %>% 
#   left_join(p_tibble_extrap %>% 
#               select(age, HD, UD),
#             by = "age") %>% 
#   mutate(deaths = ifelse(current_state == "H", f * HD, f * UD)) %>% 
#   group_by(ref_state,dur,age) %>% 
#   summarize(f = sum(deaths, na.rm=TRUE),.groups= "drop") 
# 
# do_this <- FALSE
# if (do_this){
#   # create the object that has all combos of age and duration for ref state H.
#   # ref_state is the one that d refers to
#   d1 <- expand.grid(dur = 0:n,
#                     age = ages,
#                     ref_state = "H",
#                     current_state = c("H","U"),
#                     f = 0, deaths = 0) %>% 
#     filter(dur <= (age - 50)) %>% 
#     # note we plug in init for age 50 
#     mutate(f = case_when(age == 50 & dur == 0 & current_state == "H" ~ .8,
#                          age == 50 & dur == 0 & current_state == "U" ~ .2,
#                          TRUE ~ 0))
#   
#   
#   
#   # age 50 is starting stock
#   for (a in (ages+1)){
#     
#     # possible durations include from 0 up until the hypothetical max
#     # for the current age. Maybe technically we should extract a-1,
#     # but it makes no difference because the highest d for age a
#     # is 0 and we strictly add.
#     possible_ds <- d1 %>% filter(age == a) %>% pull(dur) %>% unique() %>% sort()
#     possible_ds <- possible_ds[-which.max(possible_ds)]
#     
#     # now get transitions, only age-structured here
#     HH <- p_tibble_extrap %>% 
#       filter(age == a - 1) %>% 
#       pull(HH)
#     HU <- p_tibble_extrap %>% 
#       filter(age == a - 1) %>% 
#       pull(HU)
#     UU <- p_tibble_extrap %>% 
#       filter(age == a - 1) %>% 
#       pull(UU)
#     UH <- p_tibble_extrap %>% 
#       filter(age == a - 1) %>% 
#       pull(UH)
#     # current calc should go one age at a time...
#     for (d in possible_ds){
#       
#       # 'from' stocks: (previous age, current d and current state)
#       # i.e. there are two ways to be in the previous age with the current
#       # duration
#       Had <-
#         d1 %>% 
#         filter(age == a - 1, 
#                dur == d, 
#                current_state == "H") %>% 
#         pull(f)
#       
#       Uad <-
#         d1 %>% 
#         filter(age == a - 1, 
#                dur == d, 
#                current_state == "U") %>% 
#         pull(f)
#       
#       
#       d1 <-
#         d1 %>% 
#         # each step is next age (we iterate starting at 51)
#         mutate(f = case_when(
#           # if healthy and stay healthy then 
#           # promote one age and duration
#           age == a & dur == d + 1 & current_state == "H" ~ f + Had * HH,
#           # if healthy but become unhealthy, 
#           # promote to next age, but d stays the same
#           age == a & dur == d & current_state == "U" ~ f + Had * HU,
#           TRUE ~ f)) %>% 
#         mutate(f = case_when(
#           # if unhealthy but get healthy, promote age and duration
#           age == a & dur == d + 1 & current_state == "H" ~ f + Uad * UH,
#           # if unhealthy and stay unhealthy, promote age, but d stays same
#           age == a & dur == d & current_state == "U" ~ f + Uad * UU,
#           TRUE ~ f)) 
#     }
#   }
#   # sums check
#   sum(d1$f)
#   sum(lu)+sum(lh) # yay
#   
#   d1 %>% 
#     group_by(dur) %>% 
#     summarize(f=sum(f)) %>% 
#     ggplot(aes(x=dur,y=f)) + 
#     geom_line()
#   d1 %>% 
#     group_by(age, dur) %>% 
#     summarize(f=sum(f)) %>% 
#     ggplot(aes(x = age, y = f, color = dur, group = dur)) + 
#     geom_line()
#   d1 %>% 
#     group_by(age, dur) %>% 
#     summarize(f=sum(f)) %>% 
#     ggplot(aes(x = dur, y = f, color = age, group = age)) + 
#     geom_line()
#   
#   # 1-year discrepancy
# 
# }