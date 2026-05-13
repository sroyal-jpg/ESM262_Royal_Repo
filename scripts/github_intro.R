
### 4/21/26
### MAKING A GITHUB REPO

library(usethis)
usethis::git_sitrep()
### CREATES THE LOCAL REPO

install.packages("usethis")
usethis::use_git()        # initializes git
usethis::use_github()     # creates the GitHub repo and links it

# RANDOM CHANGE
library(here)
read.csv(here("data",p301.csv))

library(tidyverse)