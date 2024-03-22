library(tidyverse)
source("aux_functions.R")

# p_tibble <- read_csv("transitions_extrap.csv",
#                             show_col_types = FALSE)

# given a sequence, what is it's absolute probability?

# init <- c(H = .8, U = .2)

prob_traj <- function(traj, p_tibble, init){
  p_tibble <- as.matrix(p_tibble)
  rownames(p_tibble) <- 1:nrow(p_tibble)
 
  # traj should be of the form
  # HHHHUUHHHHHUHHHHHD, etc
  if (length(traj) == 1){
    if (nchar(traj) > 1){
      traj <- str_split(traj,"") %>% unlist()    
    }
  }
  
  selector <- 
    tibble(from = traj,
           to = lead(traj)) %>% 
    filter(!is.na(to)) %>% 
    mutate(fromto = paste0(from, to)) %>% 
    pull(fromto)
  
  select_mat <- cbind(1:length(selector),selector)
  out        <- init[traj[1]] * prod(p_tibble[select_mat])
  names(out) <- NULL
  out
}

# prob_traj("HHHHHHHHHHD", p_tibble, init)
# prob_traj("UUUUUUUUUUD", p_tibble, init)
# prob_traj("HUHUHUHUHUD", p_tibble, init)
# prob_traj("HHUUHHUUHHD", p_tibble, init)
# prob_traj("HHHUUUHHHUD", p_tibble, init)
# prob_traj("UHUHUHUHUHD", p_tibble, init)
# prob_traj("UUHHUUHHUUD", p_tibble, init)
# prob_traj("UUUHHHUUUHD", p_tibble, init)
# prob_traj("HHHHHHHHHHHHHHHHHHHHHHHHHUUUD", p_tibble, init)
# given n timesteps and s states, how many lives
# can be lived?
possible_traj <- function(ages = 30, states = 2){
  sum(states ^ (1:ages))
}

# what is the probability of a life having a total
# of N time steps that are equal to s?
prob_total_dur <- function(p_tibble, 
                           target_state = "U", 
                           dur = 1, 
                           states = c("U","H"), 
                           init = c(H=.95,U=.05)){
  n <- nrow(p_tibble)
  # could get all spell length probabilities, then
  # all the potential ways to sum to the given total 
  # duration? More efficient?
  pos <-
    combn(1:n,dur) %>% 
    t()
  base_traj <- rep(states[!states == target_state],n)
  p <- 0
  # begin longevity loop
  for (longevity in (dur+1):n){
    this_traj <- base_traj[1:longevity]
    ind       <- rowSums(pos < longevity) == dur
    inds      <- pos[ind,1:dur,drop=FALSE]
    ii <- nrow(inds)
    
    # two ways to handle
    # end first option
    # otherwise single thread:
    for (ind in 1:ii){
      if (longevity < n){
        this_traj[inds[ind,]] <- target_state
        this_traj[longevity] <-"D"  
        p                    <- p + prob_traj(this_traj, p_tibble, init)
      }
    } # end single thread loop
  }
  p
}

test_this <- FALSE
if (test_this){
  init = c(H=.95,U=.05)
  # for the first duration, this can be calculated in ~5-10 seconds
prob_total_dur(p_tibble, dur = 1, target_state = "U", init= init)
prob_total_dur(p_tibble, dur = 1, target_state = "H", init= init)
}

# for d > 1 you already have time to make coffee 
# for d > 5 you have time to watch a movie I think
do_this <- FALSE
if (do_this){
# this is painfully slow, don't even bother trying to do this
# for all durations!!!
p_dur <- rep(0,27)
for (d in 22:24){ 
  p_dur[d] <- prob_total_dur(p_tibble, dur = d, target_state = "U")
}
p_dur %>% plot()
}

enumerate_trajectories <- function(ages = 5, states = c("H","U")){
   
  out <- list()
  out[[1]] <- states
  for (i in 2:ages){
    out[[i]] <- c(sapply(out[[i-1]],paste0,states))
  }
  out <- unlist(out) %>% 
    paste0("D")
  out
}
# trajectories <- enumerate_trajectories()

plot_trajectories <- function(trajectories, p_tibble, scale = TRUE, init){
  if (is.null(names(init))){
    names(init) <- c("H","U")
  }
  p_tibble <- as.matrix(p_tibble)
  L <-str_split(trajectories, 
            pattern = "") %>% 
    lapply(function(x){
      tibble(state = x) %>% 
        mutate(age=1:n()-1)
    }) 
  data.table::rbindlist(L,idcol=TRUE) %>% 
    as_tibble() %>% 
    group_by(.id) %>% 
    mutate(height = ifelse(scale, 
                           prob_traj(traj = state, p_tibble = p_tibble, init = init),1))  %>% 
    pivot_wider(names_from=age,values_from=state) %>% 
    ungroup() %>% 
    arrange(-.id) %>% 
    mutate(y = cumsum(height)) %>%
    pivot_longer(`0`:`5`,names_to="age",values_to="state") %>% 
    filter(!is.na(state)) %>% 
    mutate(xmin=as.integer(age),
           xmax = xmin+1,
           ymax=y,
           ymin = ymax - height) %>% 
    filter(state!="D") %>% 
    ggplot(aes(xmin=xmin,xmax= xmax,ymin=ymin,ymax=ymax,fill=state)) +
    geom_rect() +
    scale_fill_manual(values=c(H=gray(.8),U=gray(.2))) +
    theme_minimal() 
}
