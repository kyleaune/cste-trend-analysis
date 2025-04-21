#---- Setup #----

# Listing all the packages that are used
# tidyverse contains lots of useful data management and manipulation packages and functions
# excessmort is the main package used for the excess calculation analysis
# ggplot creates nice figures
# tidycensus uses the census API to download demographic data (api key is required
# and can be requested here: "https://api.census.gov/data/key_signup.html")
# ISOweek helps convert week names to dates
# kableExtra converts R dataframes and tables to nice exportable objects
pkgs <- c("tidyverse", "excessmort", "ggplot2", "tidycensus", "ISOweek", "kableExtra")

# Installing any of the packages that you don't already have
install.packages(setdiff(pkgs, rownames(installed.packages())))

# Loading all the packages
lapply(pkgs, library, character.only = TRUE, quietly = TRUE, verbose = FALSE)

# Setting working directory
setwd("~/your_working_directory")

# Reading in full dataset
dat <- readRDS("deidentified_training_data_2016-2023.rda") %>%
  # Assigning each death to the week it occurred in
  mutate(isoweek = paste0(isoyear(DEATHDATE), "-W", sprintf("%02d", isoweek(dat$DEATHDATE)),"-1")) %>%
  # Listing the date of the start of the week each death occurred
  mutate(isodate = ISOweek2date(isoweek))

# Downloading demographics
# Uses the census API via 'tidycensus' (requires API key)
# You can also read in your own locally stored demographic dataset as long as it
# has columns named for each of the variables you plan to subset by in your
# full dataset
demo <- get_demographics(geography = "county", state = "Maryland", county = "510",
                         years = seq(from = min(year(dat$isodate), na.rm = TRUE),
                                     to = max(year(dat$isodate), na.rm = TRUE),
                                     by = 1),
                         vars = c("SEX", "AGEGROUP", "RACE", "HISP"))

# Checking to make sure all years are downloaded
data.frame(
  year = seq(from = min(year(dat$isodate), na.rm = TRUE),
             to = max(year(dat$isodate), na.rm = TRUE),
             by = 1)) %>%
  mutate(dl = if_else(year %in% demo$year, "Yes", "No"))
  ## 2023 not downloaded (likely not available yet)

# Fixing the missing 2023 data by setting 2023 demographic values as the same as 2022 values
demo.23 <- demo[demo$year == 2022, ] %>%
  mutate(year = 2023)
# Then adding these values to the end of the downloaded demographic file
demo <- bind_rows(demo, demo.23)

# The analysis needs a demographic record for every day in the analysis, so we
# create one here using 'excessmort::approx_demographics'
# extrapoloation.type can be linear (assume a linear change from year 1 to year 2
# and impute the change along the way) or constant (keep all days of year 1 the
# same)
demo <- approx_demographics(demo, first_day = min(dat$isodate, na.rm = TRUE),
                            last_day = max(dat$isodate, na.rm = TRUE),
                            extrapolation.type = "constant")


#---- Calculating Overall Excess Mortality #----

# Aggregating individual level data to weekly count data (and adding demographic
# data so we have population offsets for rate calculation)
all.counts <- compute_counts(dat,
                             demo = demo,
                             # We want to use weekly counts so we specify dat
                             # as the Monday of every week
                             date = "isodate")

# Creating list of dates where we want to calculate excess (i.e. during the pandemic)
# This is basically all of the dates that we don't want to train our expected counts
pandemic.dates <- seq(make_date(2020, 3, 1), max(dat$DEATHDATE, na.rm = TRUE), by = "day")

# Fitting model to calculate excess mortality at each week
weekly.xs.all <- all.counts %>%
  excess_model(
    # Telling the model not to calculate historic trends in the excluded date range
    exclude = pandemic.dates,
    # Specifying the start of the model creation. We're using the start of the full
    # dataset, if you wanted to ignore the first month/year/etc. of your data you
    # could do so here.
    start = min(.$date),
    # Specifying the end of the model. Again, if there are reasons you wouldn't want
    # to use the full datset (maybe you have low confidence in data completion for
    # recent timepoints) you can do so here.
    end = max(.$date),
    # Allowing for monthly changes in the baseline prediction model
    knots.per.year = 12,
    # Specify whether you want a linear annual trend included in the prediction.
    # Not recommended if you have 5 or fewer years of baseline data.
    include.trend = FALSE,
    # Specifying the model type and statistical significance level
    model = "quasipoisson",
    alpha = 0.05)

# Restructuring model output (list) to dataframe for ease of use (dropping stuff
# we're not interested in). Keeping the date, observed counts, expected counts,
# and standard error of the expected counts.
weekly.xs.all <- bind_cols(weekly.xs.all[1:4]) %>%
  # Calculating 95% confidence intervals of expected deaths
  mutate(exp.l95 = expected + qnorm(0.025) * exp(log_expected_se),
         exp.u95 = expected + qnorm(0.975) * exp(log_expected_se))

# Restructuring for plotting . ggplot likes data formatted in a long format, so
# we're stacking the observed and expected counts in a single column and adding
# a new label called 'class' to describe whether that number is an observed or
# expected count.
weekly.all.plotdat <- weekly.xs.all %>%
  # Setting starting time of plot
  filter(date > make_date(2020, 01, 01)) %>%
  # Stacking observed and expected to make ggplot creation easier
  pivot_longer(cols = observed:expected,
               names_to = "class",
               values_to = "value") %>%
  # Renaming and reordering to make the legend of our plot look nicer
  mutate(class = factor(class, levels = c("observed", "expected"),
                        labels = c("Observed", "Expected")))

# Plotting observed vs expected
ggplot(weekly.all.plotdat, aes(x = date, y = value, col = class)) +
  geom_line() +
  # Specifying monthly x-axis labels with minor ticks for each week
  scale_x_date(
    date_breaks = "1 month",
    date_minor_breaks = "1 week",
    date_labels = "%b %Y") +
  # Rotating x-axis labels and dropping minor grid lines in the plot panel
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  xlab("Date") +
  ylab("Weekly Death Count") +
  # Dropping the legend title (since there's only one type of presented data)
  labs(col = NULL)

# Calculating total excess by period of time
all.counts %>%
  excess_model(
    exclude = pandemic.dates,
    # Instead of specifying a start and stop date, we can specify a period(s) of
    # interest where we want to specifically calculate excess mortality. These
    # periods always have to be included as a list (even if there's only 1).
    intervals = list(
      # Full pandemic period
      pandemic.dates,
      # Year 1 of the pandemic
      seq(make_date(2020, 03, 01), make_date(2021, 02, 28), by = "day"),
      # Year 2 of the pandemic
      seq(make_date(2021, 03, 01), make_date(2022, 02, 28), by = "day"),
      # Year 3 of the pandemic
      seq(make_date(2022, 03, 01), make_date(2023, 02, 28), by = "day")),
    knots.per.year = 12,
    include.trend = FALSE,
    model = "quasipoisson",
    alpha = 0.05) %>%
  # Adding labels for the period results
  mutate(period = c("Full", "Year 1", "Year 2", "Year 3")) %>%
  # Moving the label column to the left of the table
  relocate(period) %>%
  # Outputting results as a kable object that we can save or copy/paste into Excel
  kbl()


#---- Calculating Excess Mortality by Age Group #----

# Counting deaths by week per age group
age.counts <- compute_counts(dat,
                             demo = demo,
                             date = "isodate",
                             # Specifying the name of the column in the demographic
                             # file that contains agegroup labels
                             by = "agegroup",
                             # Specifying the name of the column in the full dataset
                             # that contains numeric age
                             age = "AGE",
                             # Specifying breaks in the data where we want to create
                             # age categories (must have hard bounds on the right
                             # and left, i.e. 0 on the left and infinity on the right)
                             breaks = c(0, 20, 40, 60, 80, Inf))

# Splitting the weekly counts by age group according to age group --> results in
# a list of data frames named according to the different values in the variable
age.counts.ls <- split(age.counts, f = age.counts$agegroup)

# Fitting model to calculate excess mortality specific to each age group
weekly.xs.age <- lapply(age.counts.ls, function(x) {
  return(
    # 'lapply' loops over each item in a list and applies a function. In this case,
    # we've created a function that just turns each item in the list (i.e. each
    # data frame of age group count data) into a new object called 'x'. Then, we
    # can run the excess model on just that subset of age group counts and return
    # the results as an item in a new list.
    x %>%
      excess_model(exclude = pandemic.dates,
                   start = min(.$date),
                   end = max(.$date),
                   knots.per.year = 12,
                   include.trend = FALSE,
                   model = "quasipoisson",
                   alpha = 0.05)
  )
})

# Restructuring for plotting (stacking obs vs exp)
weekly.age.plotdat <- lapply(weekly.xs.age, function(x) {
  # Restructuring model output (list) to dataframe for ease of use (dropping stuff
  # we're not interested in)
  x <- bind_cols(x[1:4]) %>%
    # Calculating 95% confidence intervals
    mutate(exp.l95 = expected + qnorm(0.025) * exp(log_expected_se),
           exp.u95 = expected + qnorm(0.0975) * exp(log_expected_se))
  
  return(x %>%
           # Setting starting time of plot
           filter(date > make_date(2020, 01, 01)) %>%
           # Stacking observed and expected to make ggplot creation easier
           pivot_longer(cols = observed:expected,
                        names_to = "class",
                        values_to = "value") %>%
           mutate(class = factor(class, levels = c("observed", "expected"),
                                 labels = c("Observed", "Expected"))))
  })
  
# Plotting observed vs expected. 'mapply' is similar to 'lapply' but in this case
# we're looping over multiple lists (m for multiple). So we write a function where
# x represents each object in one list (the weekly.age.plotdat list in this case)
# and y represents each object in another list (the names of the weekly.age.plotdat
# list)
xs.age.plots <- mapply(function(x, y) {
  return(
    ggplot(x, aes(x = date, y = value, col = class)) +
      geom_line() +
      scale_x_date(
        date_breaks = "1 month",
        date_minor_breaks = "1 week",
        date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()) +
      xlab("Date") +
      ylab("Weekly Death Count") +
      labs(col = NULL,
           title = "Excess Mortality 2020-2023",
           # Adding a subtitle for the plots that includes the names of each
           # age category
           subtitle = paste(y, "Year Olds", sep = " "))
  )
  }, x = weekly.age.plotdat, y = names(weekly.age.plotdat), 
  # By default, 'mapply' returns a matrix/array of data by mushing all your results
  # together. We don't want to do this (we want a list of results), so we set this
  # 'simplify' option to FALSE to turn it off
  SIMPLIFY = FALSE)

# Calculating overall excess during pandemic by age group
mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(agegroup = y) %>%
           relocate(agegroup) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
  }, x = age.counts.ls, y = names(age.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  kbl()

# Calculating overall excess by period
mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates,
                                         seq(make_date(2020, 03, 01), make_date(2021, 02, 28), by = "day"),
                                         seq(make_date(2021, 03, 01), make_date(2022, 02, 28), by = "day"),
                                         seq(make_date(2022, 03, 01), make_date(2023, 02, 28), by = "day")),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(agegroup = y) %>%
           relocate(agegroup) %>%
           mutate(period = c("Full", "Year 1", "Year 2", "Year 3")) %>%
           relocate(period) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
  }, x = age.counts.ls, y = names(age.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  kbl()


#---- Calculating Excess Mortality by Race/Ethnicity #----

# Coding race
dat <- dat %>%
  mutate(race = case_when(HISPANIC == "Y" ~ "hispanic",
                          RACEASIANINDIAN == "Y" ~ "asian",
                          RACECHINESE == "Y" ~ "asian",
                          RACEFILIPINO == "Y" ~ "asian",
                          RACEJAPANESE == "Y" ~ "asian",
                          RACEKOREAN == "Y" ~ "asian",
                          RACEVIETNAMESE == "Y" ~ "asian",
                          RACEOTHERASIAN == "Y" ~ "asian",
                          RACEBLACK == "Y" ~ "black",
                          RACEWHITE == "Y" ~ "white",
                          RACEUNKNOWN == "Y" ~ NA_character_,
                          .default = "other"))

# Counting deaths by week per race group
race.counts <- compute_counts(dat,
                              demo = demo,
                              date = "isodate",
                              by = "race")

# Fitting model to calculate excess mortality
race.counts.ls <- split(race.counts, f = race.counts$race)
weekly.xs.race <- lapply(race.counts.ls, function(x) {
  return(
    x %>%
      excess_model(exclude = pandemic.dates,
                   start = min(.$date),
                   end = max(.$date),
                   knots.per.year = 12,
                   include.trend = FALSE,
                   model = "quasipoisson",
                   alpha = 0.05)
  )
})

# Restructuring for plotting (stacking obs vs exp)
weekly.race.plotdat <- lapply(weekly.xs.race, function(x) {
  # Restructuring model output (list) to dataframe for ease of use (dropping stuff
  # we're not interested in)
  x <- bind_cols(x[1:4]) %>%
    # Calculating 95% confidence intervals
    mutate(exp.l95 = expected + qnorm(0.025) * exp(log_expected_se),
           exp.u95 = expected + qnorm(0.0975) * exp(log_expected_se))
  
  return(x %>%
           # Setting starting time of plot
           filter(date > make_date(2020, 01, 01)) %>%
           # Stacking observed and expected to make ggplot creation easier
           pivot_longer(cols = observed:expected,
                        names_to = "class",
                        values_to = "value") %>%
           mutate(class = factor(class, levels = c("observed", "expected"),
                                 labels = c("Observed", "Expected"))))
})

# Plotting observed vs expected
xs.race.plots <- mapply(function(x, y) {
  return(
    ggplot(x, aes(x = date, y = value, col = class)) +
      geom_line() +
      scale_x_date(
        date_breaks = "1 month",
        date_minor_breaks = "1 week",
        date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()) +
      xlab("Date") +
      ylab("Weekly Death Count") +
      labs(col = NULL,
           title = "Excess Mortality 2020-2023",
           subtitle = paste(y, "Race/Ethnicity", sep = " "))
  )
}, x = weekly.race.plotdat, y = names(weekly.race.plotdat), SIMPLIFY = FALSE)

# Calculating overall excess during pandemic by race group
mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(agegroup = y) %>%
           relocate(agegroup) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = race.counts.ls, y = names(race.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  kbl()

# Calculating overall excess by period
mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates,
                                         seq(make_date(2020, 03, 01), make_date(2021, 02, 28), by = "day"),
                                         seq(make_date(2021, 03, 01), make_date(2022, 02, 28), by = "day"),
                                         seq(make_date(2022, 03, 01), make_date(2023, 02, 28), by = "day")),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(agegroup = y) %>%
           relocate(agegroup) %>%
           mutate(period = c("Full", "Year 1", "Year 2", "Year 3")) %>%
           relocate(period) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = age.counts.ls, y = names(age.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  kbl()


#---- Calculating Excess Mortality by Place of Death #----

# Coding place of death
dat <- dat %>%
  mutate(place = factor(DEATHPLACE,
                        levels = c(1:7, 9),
                        labels = c("Inpatient",
                                   "ER/Outpatient",
                                   "Other",
                                   "Home",
                                   "Hospice",
                                   "Nursing Home/LTCF",
                                   "Other",
                                   "Unknown")))

# Counting deaths by week per place of death group
place.counts <- compute_counts(dat,
                               demo = demo,
                               date = "isodate",
                               by = "place")

# Fitting model to calculate excess mortality
place.counts.ls <- split(place.counts, f = place.counts$place)
weekly.xs.place <- lapply(place.counts.ls, function(x) {
  return(
    x %>%
      excess_model(exclude = pandemic.dates,
                   start = min(.$date),
                   end = max(.$date),
                   knots.per.year = 12,
                   include.trend = FALSE,
                   model = "quasipoisson",
                   alpha = 0.05)
  )
})

# Restructuring for plotting (stacking obs vs exp)
weekly.place.plotdat <- lapply(weekly.xs.place, function(x) {
  # Restructuring model output (list) to dataframe for ease of use (dropping stuff
  # we're not interested in)
  x <- bind_cols(x[1:4]) %>%
    # Calculating 95% confidence intervals
    mutate(exp.l95 = expected + qnorm(0.025) * exp(log_expected_se),
           exp.u95 = expected + qnorm(0.0975) * exp(log_expected_se))
  
  return(x %>%
           # Setting starting time of plot
           filter(date > make_date(2020, 01, 01)) %>%
           # Stacking observed and expected to make ggplot creation easier
           pivot_longer(cols = observed:expected,
                        names_to = "class",
                        values_to = "value") %>%
           mutate(class = factor(class, levels = c("observed", "expected"),
                                 labels = c("Observed", "Expected"))))
})

# Plotting observed vs expected
xs.place.plots <- mapply(function(x, y) {
  return(
    ggplot(x, aes(x = date, y = value, col = class)) +
      geom_line() +
      scale_x_date(
        date_breaks = "1 month",
        date_minor_breaks = "1 week",
        date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()) +
      xlab("Date") +
      ylab("Weekly Death Count") +
      labs(col = NULL,
           title = "Excess Mortality 2020-2023",
           subtitle = paste(y, "Place of Death", sep = " "))
  )
}, x = weekly.place.plotdat, y = names(weekly.place.plotdat), SIMPLIFY = FALSE)

# Calculating overall excess during pandemic by place of death group
mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(agegroup = y) %>%
           relocate(agegroup) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = place.counts.ls, y = names(place.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  kbl()

# Calculating overall excess by period
mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates,
                                         seq(make_date(2020, 03, 01), make_date(2021, 02, 28), by = "day"),
                                         seq(make_date(2021, 03, 01), make_date(2022, 02, 28), by = "day"),
                                         seq(make_date(2022, 03, 01), make_date(2023, 02, 28), by = "day")),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(agegroup = y) %>%
           relocate(agegroup) %>%
           mutate(period = c("Full", "Year 1", "Year 2", "Year 3")) %>%
           relocate(period) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = place.counts.ls, y = names(place.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  kbl()
