
#' @title init_constant
#' @description derive initial conditions as function of probabilities in the first time step. This assumes that probabilities in ages prior to the first time step were constant.
#' @param x numeric vector with named elements `HH`, `UH`, `HU`, `UU`
#' @return numeric vector of length two with initial composition. `H` gives the fraction healthy at start, and `U` gives the fraction unhealthy at start.
init_constant <- function(x){
  u <- matrix(x[c("HH","UH","HU","UU")] %>% unlist(),2,byrow=TRUE)
  v <- eigen(u)$vectors[,1]
  init <- v / sum(v)
  names(init) <- c("H","U")
  init
}

#' @title pi_block
#' @description Produce a submatrix for a given transition type, to be composed together with other submatrices to form the full transient matrix.
#' @param p vector of transition probabilities, ordered by age
#' @param state_from character, the origin state, used for margin labeling.
#' @param state_to character, the destination state, used for margin labeling.
#' @param age vector giving the age classes (lower bounds), used for margin labeling.
#' @return a matrix of dimension `length(p)+1` by `length(p)+1`.
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
#' @title pi_block_outer
#' @description Produce a submatrix for a given transition type, to be composed together with other submatrices to form the full transient matrix. For use in tidy framework. This function wraps `pi_block()`.
#' @param chunk data.frame with columns `p`, `from`, `to`, and `age`
#' @return a tibble of dimension `length(p)+1` by `length(p)+1`.
pi_block_outer <- function(chunk) {
  
  pi_block(chunk[["p"]] |>
             as.double(),
           chunk[["from"]],
           chunk[["to"]],
           chunk[["age"]]) |>
    as_tibble()
  }

#' @title p_tibble2U
#' @description Produce the transient matrix `U` based on a tidy `data.frame` of transition probabilities.
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state.
#' @return matrix `U` composed of submatrices for each transition.
p_tibble2U <- function(p_tibble) {
  
    age <- p_tibble[["age"]] |>
      unique() |>
      sort()
    
    age <- c(age, age[length(age)] + 1)
    pre <- p_tibble |>
      dt_pivot_longer(-age, 
                   names_to  = "fromto", 
                   values_to = "p") |>
      fmutate(from = substr(fromto, 0, 1),
             to   = substr(fromto, 2, 2)) |>
      fselect(-fromto) |>
      fsubset(to != "D")
    
    states <- pre$from |>
      unique()
    
    pre |>
      group_by(from, to) |>
      nest() |>
      fmutate(data = map(data, ~ .x |>
                          pi_block_outer())) |> 
      dt_pivot_wider(names_from  = from, 
                  values_from = data) |> 
      unnest(cols = all_of(states),
             names_sep = "") |> 
      ungroup() |> 
      fmutate(to = paste(rep(states, each = length(age)), 
                        rep(age,    length(states)), # each
                        sep = "_")) |> 
      column_to_rownames("to") |> 
      as.matrix()
    }
#' @title p_tibble2N
#' @description Produce the fundamental matrix `N` based on a tidy `data.frame` of transition probabilities.
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state.
#' @return the fundamental matrix, `N`, containing age-state conditional survivorship values

p_tibble2N <- function(p_tibble, discount = FALSE) {
  
  U <- p_tibble2U(p_tibble)
  I <- diag(rep(1, nrow(U)))
  N <- solve(I - U) 
  if (discount) {
    N < N - I / 2
  }
  return(N)
  }

#' @title p_tibble2lxs
#' @description produce a tidy data.frame of age-state survivorships for a given `state`, duly weighted by some declared initial conditions, `init`.
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state.
#' @param state character for which state we want `lxs`. Presumably but not necessarily `"H"` or `"U"`
#' @param init numeric vector giving initial conditions. Elements should be labelled with the state shorthand used, presumably but not necessarily `"H"` and `"U"`.
#' @return data.frame giving the age-specific values of survivors for the given state

p_tibble2lxs <- function(p_tibble, state = "H", init = c(H = 0.8, U = 0.2)) {
  
  age                 <- p_tibble[["age"]] |> min()
  N                   <- p_tibble2N(p_tibble)
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
    fmutate(age = parse_number(age)) |>
    dt_pivot_longer(-age, 
                 names_to  = "init_state", 
                 values_to = "lxs") |>
    left_join(init, by = "init_state") |> 
    fgroup_by(age) |>
    fsummarize(lxs = sum(lxs * init)) |> 
    fungroup()
}

#' @title calc_ex_simple
#' @description perform direct multistate lifetable calculations using increment-decrement lifetable method. 2 transients states only, using `H` and `U` as labels.
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state. Here transitions `HH`,`UU`,`UH`,and `HU` are explicitly used, so use these names.
#' @return a tibble with one row and three columns: `HLE`, `ULE`, `LE`
#' 
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
  # experimental: (how does this compare w dudel discount?)
  lh[1] <- lh[1] * .5
  lu[1] <- lu[1] * .5
  
  HLE <- sum(lh)
  ULE <- sum(lu)
 
  # H <- HLE / (HLE + ULE)
  # HLE <- HLE - .5*H
  # ULE <- ULE - .5*(1-H)
  tibble(HLE = HLE,
         ULE = ULE,
         LE  = HLE + ULE)
}
#' @title calc_dxh
#' @description perform direct multistate lifetable calculations to derive deaths structured by state-at-death (`current_state`), cumulative years lived in good health (`h`), in poor health (`u`)
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state. Here transitions `HH`,`UU`,`UH`,and `HU` are explicitly used, so use these names.
#' 
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
    mutate(qx  = ifelse(age == max(age), 1, qx),
           dxs = qx * l,
           x   = age - 50,
           u   = x - h) |> 
    select(current_state, age, x, h, u, lxsc = l, dxsc = dxs)
  d_out
}

