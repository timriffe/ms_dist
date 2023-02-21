init_constant <- function(x){
  u <- matrix(x[c("HH","UH","HU","UU")] %>% unlist(),2,byrow=TRUE)
  v <- eigen(u)$vectors[,1]
  init <- v / sum(v)
  names(init) <- c("H","U")
  init
}

pi_block <- function(p, state_from, state_to, age){
  state_fromi  <- state_from[1]
  state_toi    <- state_to[1]
  
  age         <- c(age, max(age) + 1)
  P           <- diag(p)
  P           <- cbind(rbind(0, P), 0)
  from_names  <- paste(state_fromi, age, sep = "_")
  to_names    <- paste(state_toi, age, sep = "_")
  dimnames(P) <- list(to_names, from_names)
  
  P
}

pi_block_outer <- function(chunk){
  pi_block(chunk[["p"]] %>% as.double(),
           chunk[["from"]],
           chunk[["to"]],
           chunk[["age"]])  %>% 
    # as.data.frame() %>% 
    # rownames_to_column(var = "_age") %>% 
    as_tibble()
}
Ptibble2U <-
  function(Ptibble){
    age <- Ptibble[["age"]] %>% unique() %>% sort()
    age <- c(age, age[length(age)]+1)
    pre <-
      Ptibble %>% 
      pivot_longer(-age, names_to="fromto", values_to = "p") %>% 
      mutate(from = substr(fromto,0,1),
             to=substr(fromto,2,2)) %>% 
      select(-fromto) %>% 
      filter(to != "D") 
    states <- pre$from %>% unique()
    pre %>% 
      group_by(from, to) %>% 
      nest() %>% 
      mutate(data = map(data,~.x %>% pi_block_outer())) %>% 
      pivot_wider(names_from = from, values_from = data)  %>% 
      # unnest()
      unnest(cols = all_of(states),
             names_sep = "") %>% 
      ungroup() %>% 
      
      mutate(to = paste(rep(states,each=length(age)), rep(age,length(states)), sep = "_")) %>%
      column_to_rownames("to") %>% 
      as.matrix()
  }

Ptibble2N <- function(Ptibble, discount = FALSE){
  U <- Ptibble2U(Ptibble)
  I <- diag(rep(1,nrow(U)))
  N <- solve(I - U) 
  if (discount){
    N < N - I / 2
  }
  N
}

Ptibble2lxs <- function(Ptibble,state = "H",init = c(H=.8,U=.2)){
  age  <- Ptibble[["age"]] %>% min()
  N    <- Ptibble2N(Ptibble)
  cols <- grepl(colnames(N), pattern = age)
  rows <- grepl(rownames(N), pattern = state)
  to_weight <- N[rows,cols] 
  colnames(to_weight) <- substr(colnames(to_weight),0,1)  
  
  init <-
    as.matrix(init) %>% 
    as.data.frame() %>% 
    rownames_to_column("init_state") %>% 
    rename(init = V1)
  
  to_weight %>% 
    as.data.frame() %>% 
    rownames_to_column("age") %>% 
    as_tibble() %>% 
    mutate(age = parse_number(age)) %>% 
    pivot_longer(-age, names_to = "init_state", values_to = "lxs") %>% 
    left_join(init,by="init_state") %>% 
    group_by(age) %>% 
    summarize(lxs = sum(lxs * init), .groups = "drop")
}
