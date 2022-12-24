library(tidyverse)
source("aux_functions.R")

p_tibble_extrap <- read_csv("transitions_extrap.csv",
                            show_col_types = FALSE)
# step 1: count spells by starting age.

p_tibble_extrap %>% nrow()
# 61 ages, now brute force impossible.
# The brute force approach won't be practical, as there
# are too many probabilities to derive. Each possible
# life has a calculable probability, 
# but some can be reached in too many ways.

lhx <- Ptibble2lxs(p_tibble_extrap, "H")
lux <- Ptibble2lxs(p_tibble_extrap, "U")

lu <- lux$lxs
lh <- lhx$lxs

# New spells of H must start from U or initial age of H


hh <- p_tibble_extrap[["HH"]]
uh <- p_tibble_extrap[["UH"]]

# spell termination probabilities
hend <- c(1 - hh, 1) # 1 for closeout
# we have n+1 elements (up toa ge 111)
n    <- nrow(p_tibble_extrap) # only to age 110


Hspells            <- matrix(0, n+1, n+1)
# only first row refers to people whose initial state was H. They are of course
# left censored, which is disappointing, but not really a major problem; treat
# as truncation of both lives and spells.

# Hspells aigns on spell duration
Hspells[1, ]       <- lh[1] * hend * c(1,cumprod(hh))

# Hspells_check aligns on age, but same values, just accumulated Tx-style
Hspells_check      <- matrix(0, n+1, n+1)
Hspells_check[1, ] <- lh[1] * c(1, cumprod(hh))
for (i in 2:n){
  Hspells[i,1:(n+2-i)] <- lu[i-1] * uh[i-1] * hend[i:(n+1)] * c(1,cumprod(hh[i:n]))
  Hspells_check[i, i:(n+1)] <- (lu[i-1] * uh[i-1] * c(1,cumprod(hh[i:n]))) 
}

# identical
sum(lh) # expectancy in usual way
sum(Hspells_check) # as sum of spell survival conditional on starting age
sum(colSums(Hspells) * 1:62) # as sum of spell duration conditional on starting age
# 1:62 constitutes the durations, where large numbers have small weights.

# Now note, the same episode weight divides up over the very many different lives
# that might contain it. For instance,

# of the H episodes starting in age 65 of duration 2, there will be some
# some are HHHHU-HH, others HHHUU-HH and so on in many different combinations. And differences may be either before or after.

# We want to be able to stitch together lives to get the cumulative occupancy time
# distribution, and this will require some trick.

# Each spell weight in Hspells considers spell termination due to all forms of attrition. So, in principle, we can take integer combinatorics to arrive at the lives.

# iterate over length of life to filter the possible columns of Hspells; we should be taking natural sums of spell probabilities by length of life, where each spell could be counted multiply.

# in this end the combinatorics makes this approach impractical. Using this spells by duration and onset age approach does not really reduce the size of the problem.
