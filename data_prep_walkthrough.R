#-------------------------------------------------------------------------#
#               2025 CSTE Applied Trend Analysis in R                     #
#  Data Preparation and Trend Analysis - Demonstration                    #
# Authors: Johns Hopkins Surveillance, Outbreak, and Response Team        #
#-------------------------------------------------------------------------#

#---- Set-up #----

# Summary of packages used
# excessmort is the main package used for the excess calculation analysis
# tidyverse contains lots of useful data management and manipulation packages and functions
# ggplot2 creates nice figures
# tidycensus uses the census API to download demographic data (api key is required
# and can be requested here: "https://api.census.gov/data/key_signup.html")
# ISOweek helps convert week names to dates
# kableExtra converts R dataframes and tables to nice exportable objects
pkgs <- c("excessmort", "tidyverse", "ggplot2", "tidycensus", "ISOweek", "kableExtra")

# Installing any of the packages that you don't already have
install.packages(setdiff(pkgs, rownames(installed.packages())))

# Loading all the packages
lapply(pkgs, library, character.only = TRUE, quietly = TRUE, verbose = FALSE)

# Setting working directory
# setwd("~/your_working_directory")
# DELETE FOR TRAINING # 
setwd("/Users/kathrynhogan/Library/CloudStorage/OneDrive-JohnsHopkins/2. Hopkins Year 1 Research/SORT/CSTE training/CSTE_training")

# Reading in full dataset of line list deaths in Michigan 2010-2020
mort.dat <- readRDS("linelist_2010-2020_mi.rda")
# View(mort.dat)

# Current data includes the date of death for each person
# Since we want to run the analysis over weekly counts deaths, 
# we need to figure out which week each death occurred in. 
# We'll assign each date of death to a week of death called isoweek
mort.dat <- mort.dat %>%
  mutate(isoweek = paste0(isoyear(date), "-W", sprintf("%02d", isoweek(mort.dat$date)),"-1")) %>%
  # Pulling the date of the start of the week each death occurred
  mutate(isodate = ISOweek2date(isoweek)) %>%
  # Drop the 'date' column so it doesn't confuse any of our other
  # functions later in the analysis
  select(-date) %>%
  # Because we're counting by week, the start and end of our dataset don't represent
  # a complete week (1/1/2010 was a Friday, 12/31/2020 was a Thursday), which means
  # that mortality rates for these weeks are going to look lower. We'll drop data
  # from these weeks here, but if you want to analyze a full year of your real
  # data at a weekly level, make sure you have the complete weeks at the beginning
  # and end.
  filter(isoweek != "2009-W53-1" & isoweek != "2020-W53-1")

# Details about code for assigning a date to an ISO week
# isoyear(date) gives the ISO 8601 year
# isoweek(date) gives the ISO week number (1-53)
# the format of ISO 8601 week date format is YYYY-Www-D
# W indicates that what follows is a week number
# ww is the week number (1-53)
# D is day of the week (1 = Monday, 7 = Sunday)
# sprintf pads week numbers with leading zeros if needed
# paste0 brings all these parts together to make the date

#---- Download Population Counts #----

# We want to analyze trends in mortality according to cause of death and county,
# so we need population totals for each of these groups so we can calculate group-
# specific rates of death. These population totals can also change over the time period
# of interest (2010 - 2020), so we should make sure we have fresh numbers for each
# year to account for those potential differences over time.

# For cause of death, we'll consider the entire population to be at risk, so we
# can use the same population total data from the full analysis. For certain outcomes,
# pregnancy and perinatal deaths, for example, this wouldn't be true so we would
# want to use only female population (or whatever specific population is at risk
# for the outcome under study), but to simplify things here, we'll just just the
# whole population with the assumption that the sex ratio (i.e. the # of males vs.
# females in Michigan) did not change significantly from 2010-2020.

# You can either read in your own locally stored demographic dataset as long 
# as it has columns named for each of the variables you plan to 
# subset by in your full dataset

# For today, just download the prepared population dataset called population_data.csv:
demo.total.in <- read.csv("population_data.csv")

# If you don't have the data you can download demographic data using census API key
# Uses the census API via 'tidycensus' (requires you to get an API key)
# The code below is for reference on how to use the API key to pull census data

# # Set your Census API key if you haven't already
# census_api_key("INSERT YOUR API KEY HERE", 
#                install = TRUE, 
#                overwrite = TRUE)
# 
# # Create a dataframe to hold population data from 2010–2020 for all MI counties
# years <- 2009:2020
# 
# # In excessmort the demo df has the populations sizes for each time point 
# # for each of the groups that we will compute counts for
# # initialize the df below, and add population counts to it
# demo.total.in <- data.frame()
# 
# # The for loop below uses Census API to obtain population counts for 
# # population offsets, it loops through all the years using the decennial for 
# # 2010 and 2020, and the ACS 1 year populations for all other years
# for (yr in years) {
#   
#   if (yr == 2010) {
#     # 2010 Decennial Census (SF1)
#     pop_data <- get_decennial(
#       geography = "county",
#       state = "MI",
#       variables = "P001001",  # Total population
#       sumfile = "sf1",
#       year = 2010,
#       key = "e874508679a8a42bf951f7d9706887e0669b42ce",
#       output = "wide"
#     ) %>%
#       mutate(year = yr, population = as.numeric(P001001)) %>%
#       select(GEOID, NAME, year, population)
#     
#   } else if (yr == 2020) {
#     # 2020 Decennial Census (PL 94-171 redistricting file)
#     pop_data <- get_decennial(
#       geography = "county",
#       state = "MI",
#       variables = "P1_001N",  # Total population
#       sumfile = "pl",
#       year = 2020,
#       key = "e874508679a8a42bf951f7d9706887e0669b42ce",
#       output = "wide"
#     ) %>%
#       mutate(year = yr, population = as.numeric(P1_001N)) %>%
#       select(GEOID, NAME, year, population)
#     
#   } else {
#     # ACS 1-year estimates for other years (2011–2019)
#     pop_data <- get_acs(
#       geography = "county",
#       state = "MI",
#       variables = "B01001_001E",  # Total population
#       survey = "acs1",
#       year = yr,
#       key = "e874508679a8a42bf951f7d9706887e0669b42ce",
#       output = "wide"
#     ) %>%
#       mutate(year = yr, population = as.numeric(B01001_001E)) %>%
#       select(GEOID, NAME, year, population)
#   }
#   
#   # pulls all data into a dataframe called demo which we will use
#   demo.total.in <- bind_rows(demo, pop_data)
# }

# Checking to make sure all years are downloaded
data.frame(
  year = seq(from = min(year(mort.dat$isodate), na.rm = TRUE),
             to = max(year(mort.dat$isodate), na.rm = TRUE),
             by = 1)) %>%
  mutate(dl = if_else(year %in% demo.total.in$year, "Yes", "No"))

#---- Extrapolate Population Counts #----

# Now we need to convert this table of annual population into a long list of population
# size for each day in our line list. We'll use the 'approx_demographics' function
# from the 'excessmort' package
demo.total <- approx_demographics(
  # Name the original demographic table
  demo = demo.total.in,
  # Define the first date that deaths occurred (since we're analyzing at the week
  # level, this is actually the first date of the first week deaths occurred)
  first_day = min(mort.dat$isodate),
  # Define the last date
  last_day = max(mort.dat$isodate),
  # Since we don't have population data for each day or week of each year,
  # we use the population data we do have to extrapolate the time in between. 
  # extrapoloation.type can be constant (i.e. each day of 2015 would have 
  # the same population total until the number is updated with 2016 data), 
  # or can be linear (assume a linear change from year 1 to year 2 and impute the 
  # change along the way). In this case we need to fill in the 
  # gaps so we'll use a linear extrapolation that will assume the population 
  # changed at a fixed rate between the times that we've provided in the 
  # 'demo.total.in' table.
  extrapolation.type = "linear")

#---- Identify Exclusion Period and Aggregate to Weekly Data #----

# We want to evaluate whether there were excess deaths during the pandemic, so
# we need to define the exclusion period for our analysis just like for the full
# dataset. The first COVID-19 case was reported in Michigan on March 10, 2020
# We will create a list of dates after which we want to calculate excess
# This is basically all of the dates that we don't want to train our expected counts on
pandemic.dates <- seq.Date(as.Date("2020-03-10"), 
                           as.Date("2020-12-31"), 
                           by = "day")

# Finally, we need to aggregate the individual level data into weekly count data 
# and we use compute_counts() to do so which is part of the excessmort package
all.counts <- compute_counts(dat = mort.dat, # df with individual records
                             demo = demo.total,
                             # We want to use weekly counts so we specify date
                             # as the Monday of every week
                             date = "isodate")














