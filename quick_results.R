# Quick generation of final tables and figures for (1) overall analysis of excess
# deaths in 2020, (2) subgroup analysis by cause of death, (3) subgroup analysis
# of age, (4) subgroup analysis of race

pkgs <- c("tidyverse", "excessmort")
lapply(pkgs, library, character.only = TRUE)

dat <- readRDS("linelist_2010-2020_mi.rda")

