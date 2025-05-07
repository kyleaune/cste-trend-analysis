#----------------------------------------------------------------------------#
# Title:   CDC Wonder Data Combination                                       #
# Authors: Johns Hopkins Surveillance, Outbreak, and Response Team           #
# Date:    June 8, 2025                                                      #
# Purpose: CSTE 2025 Annual Meeting; Workforce Development Workshop; Applied #
#          Trend Analysis in R                                               #
#----------------------------------------------------------------------------#


# This training uses real mortality records reported to the CDC by state health
# authorities in Michigan (the host state of the 2025 CSTE Annual Meeting), but,
# to protect the privacy of decedents and their surviving family members, aggregates
# the data into tables organized by month, county, and general demographic features.
# For this training data to represent real public health surveillance records typically
# created and used by local and state public health practitioners, this R script
# will take the monthly mortality data downloded from CDC Wonder (https://wonder.cdc.gov/)
# and


#---- Setup #----

pkgs <- c("tidyverse", "doParallel", "tidycensus")
lapply(pkgs, library, character.only = TRUE)

setwd("/Users/kyleaune/Library/CloudStorage/OneDrive-SharedLibraries-JohnsHopkins/SORT - CSTE Training/CDC Wonder Data for Training")

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
cause <- lapply(yrs, function(x) {
  return(
    read.csv(paste0(x, "/ucod_", x, ".csv")) %>%
      # Removing notes
      select(-any_of("Notes")) %>%
      # Removing entry for "codes for special purposes" (all empty rows)
      filter(UCD...ICD.Chapter != "Codes for special purposes") %>%
      # Adding short names for cause of death
      mutate(
        cod = case_match(
          UCD...ICD.Chapter,
          "Certain conditions originating in the perinatal period" ~ "perinatal",
          "Certain infectious and parasitic diseases" ~ "id",
          "Congenital malformations, deformations and chromosomal abnormalities" ~ "genetic",
          "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" ~ "blood",
          "Diseases of the circulatory system" ~ "cvd",
          "Diseases of the digestive system" ~ "gi",
          "Diseases of the ear and mastoid process" ~ "ear",
          "Diseases of the eye and adnexa" ~ "eye",
          "Diseases of the genitourinary system" ~ "gu",
          "Diseases of the musculoskeletal system and connective tissue" ~ "msk",
          "Diseases of the nervous system" ~ "nervous",
          "Diseases of the respiratory system" ~ "respiratory",
          "Diseases of the skin and subcutaneous tissue" ~ "skin",
          "Endocrine, nutritional and metabolic diseases" ~ "endocrine",
          "External causes of morbidity and mortality" ~ "external",
          "Mental and behavioural disorders" ~ "mental",
          "Neoplasms" ~ "cancer",
          "Pregnancy, childbirth and the puerperium" ~ "pregnancy",
          "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" ~ "unk",
          .default = NA_character_
        )
      ) %>%
      # Removing rows with no information
      filter(Month != "")
  )
})

# Checking imports - should have 83 counties * 12 months * 11 years = 10956 rows
  # 43824 for race (4 groups)
  # 208164 for cause (19 groups)
lapply(list(all, age1, age2, race, cause), function (x)
  nrow(bind_rows(x)))
## All ok
# Checking column names
lapply(age1, names) ## Missing 'age_group' in #5 (2014)
lapply(age2, names) ## Missing 'age_group' in #5 (2014)
lapply(race, names) ## All ok
lapply(cause, names) ## All ok

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
age <- mapply(
  FUN = function(x, y) {
    return(
      z <- bind_rows(x, y) %>%
        pivot_wider(
          names_from = age_group,
          values_from = Deaths,
          names_prefix = "d."
        )
    )
  },
  x = age1,
  y = age2,
  SIMPLIFY = FALSE
)


#---- Age #----

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
          d.18_64:d.65plus, ~ if_else(n.sup == 1,
                                      replace_na(.x, d.total - sum(c_across(d.18_64:d.65plus), na.rm = TRUE)),
                                      .x)
        )) %>%
        # When both categories are suppressed, randomly assign 65+ a number between 1
        # and total deaths
        mutate(d.65plus = if_else(
          is.na(d.65plus), sample(1:d.total, size = 1), d.65plus
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


#---- Race #-----

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
        rowwise() %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.ind:d.white))),
          d.count = sum(c_across(d.ind:d.white), na.rm = TRUE)) %>%
        # For rows with only one suppressed total, assign as difference between sum of
        # subcategories and total
        mutate(across(
          d.ind:d.white, ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.ind:d.white), na.rm = TRUE
          )), .x)
        )) %>%
        # Updating suppressed count
        mutate(
          n.sup = sum(is.na(c_across(d.ind:d.white))),
          d.count = sum(c_across(d.ind:d.white), na.rm = TRUE)) %>%
        # Assigning white deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.white = case_when(
            n.sup == 1 ~ replace_na(d.white, d.total - d.count),
            n.sup != 1 & d.total - d.count > 0 ~ replace_na(d.white,
                                                            sample(seq(1,
                                                                       min(d.total - d.count, 9)),
                                                                   size = 1)),
            n.sup != 1 & d.total - d.count <= 0 ~ replace_na(d.white, 0),
            .default = d.white)) %>%
        # Updating suppressed count
        mutate(n.sup = sum(is.na(c_across(d.ind:d.white))),
               d.count = sum(c_across(d.ind:d.white), na.rm = TRUE)) %>%
        # Assigning Black deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.black = case_when(
            n.sup == 1 ~ replace_na(d.black, d.total - d.count),
            n.sup != 1 & d.total - d.count > 0 ~ replace_na(d.black,
                                                            sample(seq(1,
                                                                       min(d.total - d.count, 9)),
                                                                   size = 1)),
            n.sup != 1 & d.total - d.count <= 0 ~ replace_na(d.black, 0),
            .default = d.black)) %>%
        # Updating suppressed count
        mutate(n.sup = sum(is.na(c_across(d.ind:d.white))),
               d.count =  sum(c_across(d.ind:d.white), na.rm = TRUE)) %>%
        # Assigning Asian deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.api = case_when(
            n.sup == 1 ~ replace_na(d.api, d.total - d.count),
            n.sup != 1 & d.total - d.count > 0 ~ replace_na(d.api,
                                                            sample(seq(1,
                                                                       min(d.total - d.count, 9)),
                                                                   size = 1)),
            n.sup != 1 & d.total - d.count <= 0 ~ replace_na(d.api, 0),
            .default = d.api)) %>%
        # Assign American Indian deaths as difference between total and assigned
        mutate(d.ind = replace_na(
          d.ind, d.total - sum(c_across(d.ind:d.white), na.rm = TRUE))) %>%
        mutate(tot = sum(c_across(d.ind:d.white))) %>%
        # Updating suppressed count
        mutate(n.sup = sum(is.na(c_across(d.ind:d.white))),
               d.count = sum(c_across(d.ind:d.white), na.rm = TRUE)) %>%
        ungroup()
    )
  },
  race = race,
  all = all,
  SIMPLIFY = FALSE
)


#---- Cause of Death #----

# Calculating overall rates of each cause (for generating suppressed counts)
bind_rows(cause) %>%
  group_by(cod) %>%
  mutate(Deaths = as.numeric(if_else(
    Deaths == "Suppressed", as.character(sample(1:5, 1)), Deaths
  ))) %>%
  summarise(total = sum(Deaths)) %>%
  mutate(rate = total / sum(total) * 100) %>%
  arrange(desc(rate))
    #    cod         total  rate
    #  1 cvd         335002 32.3
    #  2 cancer      211464 20.4
    #  3 respiratory 101446  9.79
    #  4 external     78883  7.61
    #  5 nervous      71425  6.89
    #  6 endocrine    54592  5.27
    #  7 mental       50306  4.86
    #  8 gi           33081  3.19
    #  9 gu           24911  2.40
    # 10 id           15730  1.52
    # 11 unk          14392  1.39
    # 12 msk          14209  1.37
    # 13 blood        11438  1.10
    # 14 perinatal     9810  0.947
    # 15 skin          6765  0.653
    # 16 genetic       2220  0.214
    # 17 pregnancy      328  0.0317
    # 18 ear             87  0.00840
    # 19 eye             66  0.00637

# Generating suppressed numbers for cause of death
cause <- mcmapply(
  FUN = function(cause, all) {
    return(
      cause %>%
        # Dropping long cause columns
        select(-UCD...ICD.Chapter, -UCD...ICD.Chapter.Code) %>%
        pivot_wider(
          names_from = cod,
          values_from = Deaths,
          names_prefix = "d."
        ) %>%
        mutate(across(
          d.id:d.external, ~ na_if(.x, "Suppressed")
        )) %>%
        mutate(across(d.id:d.external, ~ as.numeric(.x))) %>%
        # Adding monthly total deaths by county
        left_join(all[, c("County", "Month", "Deaths")], by = c("County", "Month")) %>%
        rename(d.total = Deaths) %>%
        rowwise() %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning cardiovascular deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.cvd = case_when(
            n.sup == 1 ~ replace_na(d.cvd, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.cvd, sample(seq(
                1, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning cancer deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.cancer = case_when(
            n.sup == 1 ~ replace_na(d.cancer, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.cancer, sample(seq(
                1, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning respiratory deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.respiratory = case_when(
            n.sup == 1 ~ replace_na(d.respiratory, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.respiratory, sample(seq(
                1, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning external cause deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.external = case_when(
            n.sup == 1 ~ replace_na(d.external, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.external, sample(seq(
                1, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning nervous system deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.nervous = case_when(
            n.sup == 1 ~ replace_na(d.nervous, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.nervous, sample(seq(
                1, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning endocrine deaths as either remaining total if only one suppressed
        # category, or random value between 1 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.endocrine = case_when(
            n.sup == 1 ~ replace_na(d.endocrine, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.endocrine, sample(seq(
                1, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning mental health deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.mental = case_when(
            n.sup == 1 ~ replace_na(d.mental, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.mental, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning gastrointestinal deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.gi = case_when(
            n.sup == 1 ~ replace_na(d.gi, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.gi, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning genitourinary deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.gu = case_when(
            n.sup == 1 ~ replace_na(d.gu, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.gu, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning infectious disease deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.id = case_when(
            n.sup == 1 ~ replace_na(d.id, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.id, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning musculoskeletal deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.msk = case_when(
            n.sup == 1 ~ replace_na(d.msk, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.msk, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning blood/immune deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.blood = case_when(
            n.sup == 1 ~ replace_na(d.blood, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.blood, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning perinatal deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.perinatal = case_when(
            n.sup == 1 ~ replace_na(d.perinatal, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.perinatal, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning skin deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.skin = case_when(
            n.sup == 1 ~ replace_na(d.skin, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.skin, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning genetic deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.genetic = case_when(
            n.sup == 1 ~ replace_na(d.genetic, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.genetic, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning pregnancy deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.pregnancy = case_when(
            n.sup == 1 ~ replace_na(d.pregnancy, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.pregnancy, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning ear deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.ear = case_when(
            n.sup == 1 ~ replace_na(d.ear, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.ear, sample(seq(
                0, min(d.total - d.count, 9)
              ), size = 1))
          )
        ) %>%
        # Counting total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # For rows with 1 suppressed total, assign as difference between sum
        # of subcategories and total
        mutate(across(
          .cols = d.id:d.external,
          .fns = ~ if_else(n.sup == 1, replace_na(.x, d.total - sum(
            c_across(d.id:d.external), na.rm = TRUE
          )), .x)
        )) %>%
        # Counting suppressed causes and total assigned deaths
        mutate(
          n.sup = sum(is.na(c_across(d.id:d.external))),
          d.count = sum(c_across(d.id:d.external), na.rm = TRUE)
        ) %>%
        # Assigning eye deaths as either remaining total if only one suppressed
        # category, or random value between 0 and lowest of remaining total or 9
        # suppressed category remaining
        mutate(
          d.eye = case_when(
            n.sup == 1 ~ replace_na(d.eye, d.total - d.count),
            n.sup != 1 &
              d.total - d.count > 0 ~ replace_na(d.eye, sample(seq(
                0, min(d.total - d.count, 9)
                ), size = 1))
            )
          ) %>%
        # Assigning remaining deaths as unknown
        mutate(d.unk = d.total - sum(c(c_across(d.id:d.genetic), d.external), na.rm = TRUE)) %>%
        ungroup() %>%
        # Cause of death appears to have had additional causes suppressed, so changing any
        # categories that are still suppressed (even though the correct number of deaths
        # has been assigned to all causes) to zero
        mutate(
          across(d.id:d.external,
                 ~ replace_na(.x, 0))) %>%
        ungroup()
    )
  },
  cause = cause,
  all = all,
  SIMPLIFY = FALSE,
  mc.cores = detectCores() - 1
)


#---- Combining Subgroups #----

# Combining age and race into single dataframe
demo <- mapply(
  FUN = function(race, age, cause) {
    return(
      race %>%
        full_join(
          age %>% select(County, Month.Code, d.lt18, d.18_64, d.65plus),
          by = c("County", "Month.Code")) %>%
        full_join(
          cause %>% select(County, Month.Code, d.id:d.external),
          by = c("County", "Month.Code")) %>%
        separate_wider_delim(Month.Code, "/", names = c("yr", "mo")) %>%
        mutate(across(yr:mo, ~ as.numeric(.x)))
    )
  },
  race = race,
  age = age,
  cause = cause,
  SIMPLIFY = FALSE
)

# Checking age, race, and cause sums
all(unlist(lapply(demo, function(x) {
  dchk <- x %>%
    mutate(asum = d.18_64 + d.65plus + d.lt18,
           rsum = d.ind + d.api + d.black + d.white) %>%
    rowwise() %>%
    mutate(csum = sum(c_across(d.id:d.external), na.rm = TRUE)) %>%
    ungroup()
  
  print(table(dchk$asum == dchk$d.total))
  print(table(dchk$rsum == dchk$d.total))
  print(table(dchk$csum == dchk$d.total))
})))
## All ok


#---- Creating Synthetic Line List #----

# Generating line list of total deaths by year
all.comb <- bind_rows(all)
all.mo <- split(all.comb, f = all.comb$Month)

all.mo <- lapply(all.mo, function(x) {
  # For each county (ii) in month x, make a new data frame with a row for every
  # death in county ii
  mox.ls <- list()
  for (ii in seq_len(nrow(x))) {
    mox.ls[[ii]] <- data.frame(county = rep(x$County[ii], x$Deaths[ii])) %>%
      # Randomly assign the date of death to within month x
      mutate(date = sample(
        seq.Date(
          from = as.Date(paste(x$yr[1], x$mo[1], "01", sep = "-")),
          to = as.Date(paste(
            x$yr[1], x$mo[1], days_in_month(as.Date(paste(
              x$yr[1], x$mo[1], "01", sep = "-"
            ))), sep = "-"
          )),
          by = "day"
        ),
        size = nrow(.),
        replace = TRUE
      ))
  }
  return(bind_rows(mox.ls))
})

# Assign death attributes by to line list month
mort.mo <- lapply(all.mo, function(x) {
  # Split month x by county
  x.c <- split(x, f = x$county)
  
  # Assign death attributes in month x by county
  out <- lapply(x.c, function(y) {
    # Subset demographic table to month x in county y
    d <- demo %>%
      bind_rows() %>%
      filter(County == y$county[1] &
               mo == month(y$date)[1] & yr == year(y$date)[1])
    
    # Create vector of ages of deaths in month x in county y
    a <- c(
      rep("<18", times = d$d.lt18),
      rep("18-64", times = d$d.18_64),
      rep("65+", times = d$d.65plus)
    )
    # Create vector of races of deaths in month x in county y
    r <- c(
      rep("American Indian", times = d$d.ind),
      rep("Asian / Pacific Islander", times = d$d.api),
      rep("Black / African American", times = d$d.black),
      rep("White", times = d$d.white)
    )
    # Create vector of causes of deaths in month x in county y
    c <- c(
      rep("Cardiovascular", times = d$d.cvd),
      rep("Cancer", times = d$d.cancer),
      rep("Respiratory", times = d$d.respiratory),
      rep("External", times = d$d.external),
      rep("Nervous System", times = d$d.nervous),
      rep("Endocrine", times = d$d.endocrine),
      rep("Mental Disorders", times = d$d.mental),
      rep("Gastrointestinal", times = d$d.gi),
      rep("Genitourinary", times = d$d.gu),
      rep("Infectious Disease", times = d$d.id),
      rep("Unknown", times = d$d.unk),
      rep("Musculoskeletal", times = d$d.msk),
      rep("Blood & Immune Disorders", times = d$d.blood),
      rep("Perinatal", times = d$d.perinatal),
      rep("Skin Disease", times = d$d.skin),
      rep("Congential Malformations", times = d$d.genetic),
      rep("Pregnancy", times = d$d.pregnancy),
      rep("Ear Disease", times = d$d.ear),
      rep("Eye Disease", times = d$d.eye)
    )
    
    # Assign ages and races of deaths in month x in county y
    y$age <- sample(a)
    y$race <- sample(r)
    y$cause <- sample(c)
    
    # Store county y of month x as list object in 'out'
    return(y)
  })
  # Store all counties in month x as list object in 'mort.mo'
  return(bind_rows(out))
})

# Full line list of Michigan mortality by county, age (+/- 65), race
mort.full <- bind_rows(mort.mo)

# Checking performance of line list generation against original CDC tables
aa <- bind_rows(age)
ra <- bind_rows(race)
ca <- bind_rows(cause)

# Age
rbind(table(mort.full$age),
      aa %>%
        summarise(across(.cols = c(d.lt18, d.18_64, d.65plus),
                         ~ sum(.x, na.rm = TRUE))))
  ## Perfect match

# Race
rbind(table(mort.full$race),
      ra %>%
        summarise(across(.cols = d.ind:d.white,
                         ~ sum(.x, na.rm = TRUE))))
  ## Perfect match

# Cause
as.data.frame(
  rbind(table(mort.full$cause),
        ca %>%
          summarise(across(.cols = c(d.blood, d.cancer, d.cvd, d.genetic, d.ear,
                                     d.endocrine, d.external, d.eye, d.gi, d.gu,
                                     d.id, d.mental, d.msk, d.nervous, d.perinatal,
                                     d.pregnancy, d.respiratory, d.skin, d.unk),
                           ~ sum(.x, na.rm = TRUE)))))
  ## Perfect match

# Saving line list
write_csv(mort.full, "linelist_2010-2020_mi.csv")
saveRDS(mort.full, "~/Documents/Research/R Projects/cste_training_workingcopy/linelist_2010-2020_mi.rda")


#---- Generating Demographics File #----

# Downloading demographics
demo <- get_demographics(geography = "county", state = "Michigan", years = 2010:2020,
                         vars = c("AGEGROUP", "RACE"))

# 2010 Decennial
demo.10 <-
  get_decennial("county",
                variables = c("P001001", paste0("P00300", 1:8), # Race
                              paste0("P012", sprintf("%03d", 1:49))), # Age
                state = "MI",
                year = 2010,
                output = "wide")

# 2015:2019 ACS 5-year (1-year incomplete for all counties)
demo.acs <- lapply(2015:2019, function(yr) {
  get_acs("county",
          variables = c("B01001A_001", "B01001B_001", "B01001C_001", "B01001D_001", "B01001E_001", # Race
                        paste0("B01001_", sprintf("%03d", 1:49))), # Age
          state = "MI",
          year = yr,
          survey = "acs5",
          output = "wide") %>%
    # Dropping margin of error columns for easier of categorizing
    select(-ends_with("M"))
})

# 2020 Decennial
demo.20 <-
  get_decennial("county",
                variables = c(paste0("DP1_", sprintf("%04d", 1:24), "C"), # Age
                              paste0("DP1_", sprintf("%04d", 78:82), "C")), # Race
                state = "MI",
                year = 2020,
                output = "wide",
                sumfile = "dp")


# Combining race and age categories
demo.10 <- demo.10 %>%
  rename(total = P003001,
         white = P003002,
         black = P003003,
         natam = P003004) %>%
  mutate(api = P003005 + P003006) %>%
  rowwise() %>%
  mutate(young = sum(c_across(P012003:P012019), c_across(P012027:P012043)),
         old = sum(c_across(P012020:P012025), c_across(P012044:P012049))) %>%
  ungroup() %>%
  select(NAME, total:natam, api:old) %>%
  mutate(year = 2010)

demo.acs <- lapply(demo.acs, function(x) {
  return(
    x %>%
      rename(total = B01001_001E,
             white = B01001A_001E,
             black = B01001B_001E,
             natam = B01001C_001E) %>%
      mutate(api = B01001D_001E + B01001E_001E) %>%
      rowwise() %>%
      mutate(young = sum(c_across(B01001_003E:B01001_019E), c_across(B01001_027E:B01001_043E)),
             old = sum(c_across(B01001_020E:B01001_025E), c_across(B01001_044E:B01001_049E))) %>%
      ungroup() %>%
      select(NAME, total, white:natam, api:old)
    )
})

demo.acs <- mapply(FUN = function(acs, yr) {
  return(
    acs %>%
      mutate(year = yr)
  )
},
acs = demo.acs,
yr = 2015:2019,
SIMPLIFY = FALSE)

demo.20 <- demo.20 %>%
  rename(total = DP1_0001C,
         white = DP1_0078C,
         black = DP1_0079C,
         natam = DP1_0080C) %>%
  mutate(api = DP1_0081C + DP1_0082C) %>%
  rowwise() %>%
  mutate(young = sum(c_across(DP1_0002C:DP1_0014C)),
         old = sum(c_across(DP1_0015C:DP1_0019C))) %>%
  ungroup() %>%
  select(NAME, total, white:natam, api:old) %>%
  mutate(year = 2020)

# Combining demographics into single dataframe
demo <- bind_rows(demo.10, demo.acs, demo.20)

# Splitting into demographics by year, county, race, and age and pivoting long
demo.total <- demo %>%
  group_by(year) %>%
  summarise(population = sum(total))
demo.co <- demo %>%
  select(year, NAME, total) %>%
  rename(county = NAME,
         population = total) %>%
  mutate(county = gsub("Michigan", "MI", county))
demo.race <- demo %>%
  group_by(year) %>%
  summarise(across(white:api, ~ sum(.x))) %>%
  pivot_longer(cols = white:api,
               values_to = "population",
               names_to = "race") %>%
  mutate(race = case_match(race,
                           "white" ~ "White",
                           "black" ~ "Black / African American",
                           "natam" ~ "American Indian",
                           "api" ~ "Asian / Pacific Islander"))
demo.age <- demo %>%
  group_by(year) %>%
  summarise(across(young:old, ~ sum(.x))) %>%
  pivot_longer(cols = young:old,
               values_to = "population",
               names_to = "age") %>%
  mutate(age = case_match(age,
                          "young" ~ "18-64",
                          "old" ~ "65+"))

# Exporting demographic tables
write_csv(demo.total, "demo_by_year_2010-2020_MI.csv")
write_csv(demo.co, "demo_by_year_by_county_2010-2020_MI.csv")
write_csv(demo.race, "demo_by_year_by_race_2010-2020_MI.csv")
write_csv(demo.age, "demo_by_year_by_age_2010-2020_MI.csv")
