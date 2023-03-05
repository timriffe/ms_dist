library(colorspace)
library(data.table)
library(tidyverse)
library(collapse) # for fsubset(), fast subsetting
library(tictoc)

init_constant <- function(x){
  u <- matrix(x[c("HH","UH","HU","UU")] %>% unlist(),2,byrow=TRUE)
  v <- eigen(u)$vectors[,1]
  init <- v / sum(v)
  names(init) <- c("H","U")
  init
}


pi_block <- function(p, state_from, state_to, age) {
  
  state_fromi  <- state_from[1]
  state_toi    <- state_to[1]
  age          <- c(age, max(age) + 1)
  P            <- diag(p)
  P            <- cbind(rbind(0, P), 0)
  from_names   <- paste(state_fromi, age, sep = "_")
  to_names     <- paste(state_toi,   age, sep = "_")
  dimnames(P)  <- list(to_names, from_names)
  return(P)
  }

pi_block_outer <- function(chunk) {
  
  pi_block(chunk[["p"]] |>
             as.double(),
           chunk[["from"]],
           chunk[["to"]],
           chunk[["age"]]) |>
    as_tibble()
  }

Ptibble2U <- function(Ptibble) {
  
    age <- Ptibble[["age"]] |>
      unique() |>
      sort()
    
    age <- c(age, age[length(age)] + 1)
    pre <- Ptibble |>
      pivot_longer(-age, 
                   names_to  = "fromto", 
                   values_to = "p") |>
      mutate(from = substr(fromto, 0, 1),
             to   = substr(fromto, 2, 2)) |>
      select(-fromto) |>
      filter(to != "D")
    
    states <- pre$from |>
      unique()
    
    pre |>
      group_by(from, to) |>
      nest() |>
      mutate(data = map(data, ~ .x |>
                          pi_block_outer())) |> 
      pivot_wider(names_from  = from, 
                  values_from = data) |>
      unnest(cols = all_of(states),
             names_sep = "") |>
      ungroup() |>
      mutate(to = paste(rep(states, each = length(age)), 
                        rep(age,    each = length(states)), # each
                        sep = "_")) |>
      column_to_rownames("to") |>
      as.matrix()
    }

Ptibble2N <- function(Ptibble, discount = FALSE) {
  
  U <- Ptibble2U(Ptibble)
  I <- diag(rep(1, nrow(U)))
  N <- solve(I - U) 
  if (discount) {
    N < N - I / 2
  }
  return(N)
  }

Ptibble2lxs <- function(Ptibble, state = "H", init = c(H = 0.8, U = 0.2)) {
  
  age                 <- Ptibble[["age"]] |> min()
  N                   <- Ptibble2N(Ptibble)
  cols                <- grepl(colnames(N), pattern = age)
  rows                <- grepl(rownames(N), pattern = state)
  to_weight           <- N[rows, cols] 
  colnames(to_weight) <- substr(colnames(to_weight), 0, 1)  
  
  init <- as.matrix(init) |>
    as.data.frame() |>
    rownames_to_column("init_state") |>
    rename(init = V1)
  
  to_weight |>
    as.data.frame() |>
    rownames_to_column("age") |>
    as_tibble() |>
    mutate(age = parse_number(age)) |>
    pivot_longer(-age, 
                 names_to  = "init_state", 
                 values_to = "lxs") |>
    left_join(init, by = "init_state") |> 
    group_by(age) |>
    summarize(lxs = sum(lxs * init), .groups = "drop")
}

calc_ex_simple <- function(p_tibble){
  init <- p_tibble |> 
    filter(age == min(age)) |> 
    init_constant()
  n    <- nrow(p_tibble)
  lu   <- c(init["U"], rep(0, n))
  lh   <- c(init["H"], rep(0, n))
  
  hh <- p_tibble$HH
  uh <- p_tibble$UH
  uu <- p_tibble$UU
  hu <- p_tibble$HU
  
  for (i in 1:n) {
    lu[i + 1] <- lu[i] * uu[i] + lh[i] * hu[i]
    lh[i + 1] <- lh[i] * hh[i] + lu[i] * uh[i]
  }
  tibble(HLE = sum(lh),
         ULE = sum(lu),
         LE = sum(lh+lu))
}

calc_dxh <- function(p_tibble){
  ages <- p_tibble$age |> unique() |> sort()
  aa   <- c(ages,max(ages)+1)
  hh   <- aa - min(aa)
  
  init <- p_tibble |> 
    filter(age == min(age)) |> 
    init_constant()
  
  d3 <- expand.grid(h = hh,
                    age = aa,
                    current_state = c("H","U"),
                    l = 0,
                    stringsAsFactors = FALSE) |>
    fsubset(h <= (age - 50)) |>
    fmutate(l = data.table::fcase(
      age == 50 & current_state == "H" , init["H"],
      age == 50 & current_state == "U" , init["U"],
      default = 0))
  for (a in ages) {
    
    d3n         <- fsubset(d3, age == a)
    possible_ds <- unique(d3n$h) |> sort()
    dt          <- fsubset(p_tibble,age == a)
    
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
  d_out <- p_tibble |>
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
  d_out
}
