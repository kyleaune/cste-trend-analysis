# Data Combination

pkgs <- c("tidyverse")

lapply(pkgs, library, character.only = TRUE)

setwd("/Users/kyleaune/Library/CloudStorage/OneDrive-SharedLibraries-JohnsHopkins/SORT - CSTE Training/CDC Wonder Data for Training/")

# Combining small data set for working copy for code development
all <- read.csv("2020/count_2020.csv")
age1 <- read.csv("2020/count_age_18to64_2020.csv")
age2 <- read.csv("2020/count_age_65plus_2020.csv")
race <- read.csv("2020/count_race_2020.csv")

age <- bind_rows(age1, age2)
