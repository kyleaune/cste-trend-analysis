#---- Setup #----

# List of package names as characters
pkgs <- c("tidyverse", "excessmort")

# Load packages
lapply(pkgs, library, character.only = TRUE)

# Read in mortatlity data set
mort.dat <- read_csv()

# We want to analyze trends in mortality according to (1) race/ethnicity, (2) age,
# (3) and sex, so we need population totals for each of these groups so we can
# calculate group-specific rates of death. These population totals can also change
# over the time period of interest (2010 - 2020), so we should make sure we have
# fresh numbers for each year to account for those potential differences over time.

# Read in population data
pop.dat <- read_csv()

# 