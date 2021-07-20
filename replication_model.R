rm(list = ls())

library(gtools)
require(haven)
require(brms)
require(kableExtra)
require(stargazer)
require(tidyverse)
require(reshape2)

options(mc.cores = parallel::detectCores())


# Read in data ------------------------------------------------------------
data <- read_csv("data.csv")

# VARIABLE DESCRIPTION:
# numsad_int: Sad reactions in post
# numhaha_int: Haha reactions in post
# numwow_int: Wow reactions
# numangry_int: Angry reactions
# numloves_int: Love reactions
# MIPP: Most Important Problem by Party
# sentiment: sentiment of the post
# log_length: length of the post (log)
# else_leader_org: category of facebook user -- "other", "leader", "organization"
# IDEO: ideology
# IDEOsq: ideology squared
# accountname: name of the account (anonymised)
# issue: issue of the post
# party_affil: party affiliation of the user

bivariate_fit_main <- brm(
  cbind(numangry_int, numloves_int) ~ MIPP * sentiment +
    log_length +
    statustype +
    else_leader_org +
    government +
    IDEO +
    IDEOsq + 
    (1 |p| accountname) + (1 |q| issue),
  data = new_sub,
  family = negbinomial(link = "log", link_shape = "log"),
  chains = 4, cores = 4, iter = 3000,
  control = list(adapt_delta = 0.999,
                 max_treedepth = 15)
)


save(bivariate_fit_main, file = "bivariate_fit_main.rda")