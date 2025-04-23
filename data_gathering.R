# Data Combination

library(tidyverse)

setwd("~SORT - CSTE Training/CDC Wonder Data for Training/")

# List of years to read in annual files
yrs <- 2010:2020

# Combining small data set for working copy for code development
all <- lapply(yrs, function(x) {
  return(read.csv(paste0(x, "/count_", x, ".csv")) %>%
           # Removing notes
           select(-Notes) %>%
           # Removing rows with no information
           drop_na(County, County.Code))
  })
age1 <- lapply(yrs, function(x) {
  return(read.csv(paste0(x, "/count_age_18to64_", x, ".csv")) %>%
           # Removing notes
           select(-Notes) %>%
           # Removing rows with no information
           drop_na(County, County.Code))
         })
age2 <- lapply(yrs, function(x) {
  return(read.csv(paste0(x, "/count_age_65plus_", x, ".csv")) %>%
           # Removing notes
           select(-Notes) %>%
           # Removing rows with no information
           drop_na(County, County.Code))
  })
race <- lapply(yrs, function(x) {
  return(read.csv(paste0(x, "/count_race_", x, ".csv")) %>%
           # Removing notes
           select(-Notes) %>%
           # Removing rows with no information
           drop_na(County, County.Code))
  })

# Checking imports (should have 83 counties * 12 months * 11 years = 10956 rows [43824 for race])
lapply(list(all, age1, age2, race), function (x) nrow(bind_rows(x)))
  ## All ok
# Checking column names
lapply(age1, names) ## Missing 'age_group' in #5 (2014)
lapply(age2, names) ## Missing 'age_group' in #5 (2014)
lapply(race, names) ## All ok

# Adding 'age_group' for age groups in 2024
age1[[5]]$age_group <- "18_64"
age2[[5]]$age_group <- "65plus"

# Splitting combined year/month 'Month.Code' column into separate year and month columns
all <- lapply(all, function(x) {
  return(
    x %>%
      separate_wider_delim(Month.Code, "/", names = c("yr", "mo")) %>%
      mutate(Deaths = gsub(
        "Suppressed", sample(1:9, size = 1), Deaths
      )) %>%
      mutate(Deaths = as.numeric(Deaths))
  )
})

# Combining age categories
age <- mapply(FUN = function(x, y) {
  return(
    z <- bind_rows(x, y) %>%
      pivot_wider(
        names_from = age_group,
        values_from = Deaths,
        names_prefix = "d."
      )
    )
  },
  x = age1, y = age2, SIMPLIFY = FALSE)

# Combining age categories & generating suppressed numbers
age <- mapply(
  FUN = function(age, all) {
    return(
      age %>%
        # Adding monthly total deaths by county to fill in suppressed data
        left_join(all[, c("County", "Month", "Deaths")], by = c("County", "Month")) %>%
        rename(d.total = Deaths) %>%
        # Changing 'Suppressed' to NA
        mutate(across(d.18_64:d.65plus, ~ as.numeric(
          na_if(.x, "Suppressed")
        ))) %>%
        # Counting total number of suppressed columns by row
        rowwise() %>%
        mutate(n.sup = sum(is.na(
          c_across(d.18_64:d.65plus)
        ))) %>%
        # For rows with only one suppressed total, assign as difference between sum of
        # subcategories and total
        mutate(across(
          d.18_64:d.65plus, ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.18_64:d.65plus), na.rm = TRUE
          )), .x)
        )) %>%
        # When both categories are suppressed, randomly assign 65+ a number between 0
        # and total deaths
        mutate(d.65plus = if_else(
          is.na(d.65plus), sample(0:d.total, size = 1), d.65plus
        )) %>%
        ungroup() %>%
        # Then assign <65 as the difference between total and 65+
        mutate(d.18_64 = if_else(
          is.na(d.18_64), d.total - d.65plus, d.18_64
        )) %>%
        # For completeness (not for analysis), define # of deaths <18 as diff between total
        # and sum of existing age groups
        rowwise() %>%
        mutate(d.lt18 = if_else(
          sum(c_across(d.18_64:d.65plus)) != d.total, d.total - sum(c_across(d.18_64:d.65plus)), 0
        )) %>%
        ungroup()
    )
  },
  age = age,
  all = all,
  SIMPLIFY = FALSE
)

# Generating suppressed numbers for race
race <- mapply(
  FUN = function(race, all) {
    return(
      race %>%
        # Creating short race name
        mutate(
          Race = case_when(
            Race == "American Indian or Alaska Native" ~ "ind",
            Race == "Asian or Pacific Islander" ~ "api",
            Race == "Black or African American" ~ "black",
            Race == "White" ~ "white"
          )
        ) %>%
        select(-Race.Code) %>%
        pivot_wider(
          names_from = Race,
          values_from = Deaths,
          names_prefix = "d."
        ) %>%
        # Adding monthly total deaths by county
        left_join(all[, c("County", "Month", "Deaths")], by = c("County", "Month")) %>%
        rename(d.total = Deaths) %>%
        mutate(across(d.ind:d.white, ~ as.numeric(
          na_if(.x, "Suppressed")
        ))) %>%
        # Counting total number of suppressed columns by row
        rowwise() %>%
        mutate(n.sup = sum(is.na(
          c_across(d.ind:d.white)
        ))) %>%
        # For rows with only one suppressed total, assign as difference between sum of
        # subcategories and total
        mutate(across(
          d.ind:d.white, ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.ind:d.white), na.rm = TRUE
          )), .x)
        )) %>%
        # Updating suppressed count
        mutate(n.sup = sum(is.na(
          c_across(d.ind:d.white)
        ))) %>%
        # Assigning white deaths as (a) difference between remainder if only one
        # suppressed category (white), or (b) between 0 and remaining total if >1
        # suppressed category remaining
        mutate(d.white = if_else(
          n.sup == 1,
          replace_na(d.white, d.total - sum(c_across(d.ind:d.white), na.rm = TRUE)),
          replace_na(d.white, sample(0:(
            d.total - sum(c_across(d.ind:d.white), na.rm = TRUE)
          ), size = 1))
        )) %>%
        # Updating suppressed count
        mutate(n.sup = sum(is.na(
          c_across(d.ind:d.white)
        ))) %>%
        # Assigning black deaths as (a) difference between remainder if only one
        # suppressed category (black), or (b) between 0 and remaining total if >1
        # suppressed category remaining
        mutate(d.black = if_else(
          n.sup == 1,
          replace_na(d.black, d.total - sum(c_across(d.ind:d.white), na.rm = TRUE)),
          replace_na(d.black, sample(0:(
            d.total - sum(c_across(d.ind:d.white), na.rm = TRUE)
          ), size = 1))
        )) %>%
        # Updating suppressed count
        mutate(n.sup = sum(is.na(
          c_across(d.ind:d.white)
        ))) %>%
        # Assigning asian deaths as (a) difference between remainder if only one
        # suppressed category (asian), or (b) between 0 and remaining total if >1
        # suppressed category remaining
        mutate(d.api = if_else(
          n.sup == 1,
          replace_na(d.api, d.total - sum(c_across(d.ind:d.white), na.rm = TRUE)),
          replace_na(d.api, sample(0:(
            d.total - sum(c_across(d.ind:d.white), na.rm = TRUE)
          ), size = 1))
        )) %>%
        # Assign american indian deaths as difference between total and assigned
        mutate(d.ind = replace_na(
          d.ind, d.total - sum(c_across(d.ind:d.white), na.rm = TRUE)
        )) %>%
        mutate(tot = sum(c_across(d.ind:d.white))) %>%
        ungroup()
    )
  },
  race = race,
  all = all,
  SIMPLIFY = FALSE
)

# Combining age and race into single dataframe
demo <- mapply(
  FUN = function(race, age) {
    return(
      race %>%
        full_join(
          age %>% select(County, Month.Code, d.lt18, d.18_64, d.65plus),
          by = c("County", "Month.Code")
        ) %>%
        separate_wider_delim(Month.Code, "/", names = c("yr", "mo")) %>%
        mutate(across(yr:mo, ~ as.numeric(.x)))
    )
  },
  race = race,
  age = age,
  SIMPLIFY = FALSE
)

# Checking age and race sums
lapply(demo, function(x) {
  x %>%
    mutate(asum = d.18_64 + d.65plus + d.lt18,
           rsum = d.ind + d.api + d.black + d.white)
  print(table(dchk$asum == dchk$d.total))
  print(table(dchk$rsum == dchk$d.total))
})
  ## All ok

# Generating line list of total deaths by year
all.comb <- bind_rows(all)
all.mo <- split(all.comb, f = all.comb$Month)

all.mo <- lapply(all.mo, function(x) {
  # For each county (ii) in month x, make a new data frame with a row for every 
  # death in county ii
  mox.ls <- list()
  for (ii in seq_len(nrow(x))) {
    mox.ls[[ii]] <- data.frame(
      county = rep(x$County[ii], x$Deaths[ii])
    ) %>%
      # Randomly assign the date of death to within month x
      mutate(date = sample(seq.Date(from = as.Date(
        paste(x$yr[1],
              x$mo[1],
              "01",
              sep = "-")),
        to = as.Date(paste(x$yr[1],
                           x$mo[1],
                           days_in_month(
                             as.Date(
                               paste(x$yr[1],
                                     x$mo[1],
                                     "01",
                                     sep = "-"))),
                           sep = "-")),
        by = "day"),
        size = nrow(.),
        replace = TRUE))
  }
  return(bind_rows(mox.ls))
}
)

# Assign death attributes by to line list month
mort.mo <- lapply(all.mo, function(x) {
  # Split month x by county
  x.c <- split(x, f = x$county)
  
  # Assign death attributes in month x by county
  out <- lapply(x.c, function(y) {
    # Subset demographic table to month x in county y
    d <- demo %>%
      bind_rows() %>%
      filter(County == y$county[1] & mo == month(y$date)[1] & yr == year(y$date)[1])
    
    # Create vector of ages of deaths in month x in county y
    a <- c(rep("<18", times = d$d.lt18),
           rep("18-64", times = d$d.18_64),
           rep("65+", times = d$d.65plus))
    # Create vector of races of deaths in month x in county y
    r <- c(rep("American Indian", times = d$d.ind),
           rep("Asian / Pacific Islander", times = d$d.api),
           rep("Black / African American", times = d$d.black),
           rep("White", times = d$d.white))
    
    # Assign ages and races of deaths in month x in county y
    y$age <- sample(a)
    y$race <- sample(r)
    
    # Store county y of month x as list object in 'out'
    return(y)
  })
  # Store all counties in month x as list object in 'mort.mo'
  return(bind_rows(out))
})

# Full line list of Michigan mortality by county, age (+/- 65), race
mort.full <- bind_rows(mort.mo)

# Saving line list
write_csv(mort.full, "linelist_2010-2020_mi.csv")
