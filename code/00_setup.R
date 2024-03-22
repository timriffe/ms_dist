
# installs prerequisites

if (!"pacman" %in% installed.packages()[,"Package"]){
  install.packages("pacman")
  
}
library(pacman)

libraries <- c("colorspace","data.table","tidyverse","collapse","tictoc","tidyfast")
if(sum(!p_isinstalled(libraries))>0) {
  p_install(
    package = libraries[!p_isinstalled(libraries)], 
    character.only = TRUE
  )
}

p_load(char = libraries, character.only = TRUE, update = FALSE)


