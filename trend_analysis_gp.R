#-------------------------------------------------------------------------#
#               2025 CSTE Applied Trend Analysis in R                     #
#                  Trend Analysis - Guided Practice                       #
# Authors: Johns Hopkins Surveillance, Outbreak, and Response Team        #
#-------------------------------------------------------------------------#

# In this guided practice session, we will ask you to use what you learned in
# the R walkthrough and apply it to a slightly altered scenario to orient 
# to the code. We will leave questions and blanks (____) in the code that 
# you will have to fill in to complete the actions. Refer to the 
# R walkthrough scripts for guidance.

#---- Scenario 1: You have fewer years of data #----

# Let's explore how excess mortality would change if you only had two years 
# of data prior to the pandemic to inform the expected number of deaths.

# Question: How do you read in the full dataset of deaths in Michigan 2010-2020
mort.dat <- readRDS("____")

# Filter out years before 2018 for session 
mort.dat <- mort.dat %>%
  filter(year(date) >= 2018)

# Question: How would you view your data to see if the filter worked?
# [write code here]

# Current data includes the date of death for each person
# Since we want to run the analysis over weekly counts deaths, 
# we need to figure out which week each death occurred in. 
# We'll assign each date of death to a week of death called isoweek
mort.dat <- mort.dat %>%
  
  # Question: Which variable are we using isoyear() and isoweek() with?
  mutate(isoweek = paste0(isoyear(______), "-W", sprintf("%02d", isoweek(_____)),"-1")) %>%
  # Pulling the date of the start of the week each death occurred
  mutate(isodate = ISOweek2date(isoweek)) %>%
  
  # Drop the 'date' column so it doesn't confuse any of our other
  # functions later in the analysis
  
  # Question: How do you drop the date column using select()?
  select(_____) %>%
  
  # Because we're counting by week, the start and end of our dataset don't represent
  # a complete week (1/1/2010 was a Friday, 12/31/2020 was a Thursday), which means
  # that mortality rates for these weeks are going to look lower. We'll drop data
  # from these weeks here, but if you want to analyze a full year of your real
  # data at a weekly level, make sure you have the complete weeks at the beginning
  # and end.
  
  # Question: If you want to make sure that the data only contains weeks within
  # 2018 to 2020, how would you adjust the following line? Do we need this line?
  filter(isoweek != "_____" & isoweek != "_____")

# To check if we need to filter the data, we can use any of the following approaches
# [write code here]

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

# Question: How do you read in the prepared population dataset?
demo.total.in <- read.csv("_____")

# Checking to make sure all years are downloaded
data.frame(
  # Question: Which variable do we use here to mark the beginning and end of the df?
  year = seq(from = min(year(mort.dat$_____), na.rm = TRUE), 
             to = max(year(mort.dat$_____), na.rm = TRUE),
             by = 1)) %>%
  mutate(dl = if_else(year %in% demo.total.in$_____, "Yes", "No"))

# Question: What years are showing in the mortality data now?
# Answer: ________

#---- Extrapolate Population Counts #----

# Now we need to convert this table of annual population into a long list of population
# size for each day in our line list. We'll use the 'approx_demographics' function
# from the 'excessmort' package
demo.total <- approx_demographics(
  # Name the original demographic table
  demo = _____,
  # Define the first date that deaths occurred (since we're analyzing at the week
  # level, this is actually the first date of the first week deaths occurred)
  first_day = min(mort.dat$_____),
  # Define the last date
  last_day = max(mort.dat$_____),
  # Since we don't have population data for each day or week of each year,
  # we use the population data we do have to extrapolate the time in between. 
  # extrapoloation.type can be constant (i.e. each day of 2015 would have 
  # the same population total until the number is updated with 2016 data), 
  # or can be linear (assume a linear change from year 1 to year 2 and impute the 
  # change along the way). In this case we need to fill in the 
  # gaps so we'll use a linear extrapolation that will assume the population 
  # changed at a fixed rate between the times that we've provided in the 
  # 'demo.total.in' table.
  extrapolation.type = "_____")

#---- Identify Exclusion Period and Aggregate to Weekly Data #----

# We want to evaluate whether there were excess deaths during the pandemic, so
# we need to define the exclusion period for our analysis just like for the full
# dataset. The first COVID-19 case was reported in Michigan on March 10, 2020
# We will create a list of dates after which we want to calculate excess
# This is basically all of the dates that we don't want to train our expected counts on
pandemic.dates <- seq.Date(as.Date("_____"), # Question: What is the start date?
                           as.Date("_____"), # Question: What is the end date?
                           by = "day")

# Finally, we need to aggregate the individual level data into weekly count data 
# and we use compute_counts() to do so which is part of the excessmort package
all.counts <- compute_counts(dat = _____, # df with individual records
                             demo = _____, # df with population data
                             # We want to use weekly counts so we specify date
                             # as the Monday of every week
                             date = "isodate")

#---- Calculating Overall Excess Mortality #----

# First, we fit a model to estimate expected weekly mortality and compare it 
# to observed deaths to determine excess mortality over time.

weekly.xs.all <- all.counts %>%
  excess_model(
    # Question: Which dates do we want to exclude from the baseline fitting?
    exclude = _____,
    
    # Define the range of data used to estimate the expected baseline.
    # Here, we use the full available dataset, but this can be adjusted if 
    # needed (e.g., to remove early noisy data or recent incomplete data).
    start = min(.$date),
    
    # Specifying the end of the model. Again, if there are reasons you wouldn't want
    # to use the full dataset (maybe you have low confidence in data completion for
    # recent timepoints) you can do so here.
    end = max(.$date),
    
    # Question: how many knots per year do we want to assign?
    knots.per.year = _____,
    
    # Specify whether you want a linear annual trend included in the prediction.
    # Not recommended if you have 5 or fewer years of baseline data.
    include.trend = _____,
    
    # Question: What type of model should we use?
    model = "_____",
    
    # Question: What significance level if we want 95% confidence interval?
    alpha = _____)

# Convert the model output (a list) into a more user-friendly data frame format.
# We're only keeping key columns: date, observed deaths, expected deaths, 
# and the standard error of the expected counts
weekly.xs.all <- bind_cols(weekly.xs.all[1:4]) %>%
  
  # Calculate the 95% confidence interval bounds for expected deaths using SE
  # Note: expected SE is on the log scale, so we exponentiate it before 
  # applying the normal quantile.
  
  # Question: What do you set qnorm to for this scenario?
  mutate(exp.l95 = expected + qnorm(_____) * exp(log_expected_se),
         exp.u95 = expected + qnorm(_____) * exp(log_expected_se))

#---- Preparing Data for Plotting ----#

# Restructuring for plotting . ggplot likes data formatted in a long format, 
# (one column for values, one for category label) so we're stacking the observed
# and expected counts in a single column and adding a new label called 'class'
# to describe whether that number is an observed or expected count.

# Question: Which data frame are we plotting?
weekly.all.plotdat <- _____ %>%
  
  # Question: Which dates will we include i the plot
  filter(date > make_date(_____)) %>%
  
  # Convert 'observed' and 'expected' columns into a single 'value' column,
  # with a new 'class' column labeling the type of value (Observed vs Expected).
  pivot_longer(cols = observed:expected,
               names_to = "class",
               values_to = "value") %>%
  
  # Renaming and reordering 'class' to make the legend of our plot look nicer
  mutate(class = factor(class, levels = c("observed", "expected"),
                        
                        # Question: What will the labels be in this case?
                        labels = c("_____", "_____")))

#---- Plotting Observed vs Expected Weekly Deaths ----#

# Plotting observed vs expected
# Questions: What dataframe are we plotting? What is the x-value?
ggplot(_____, aes(x = _____, y = value, col = class)) +
  
  # Draw lines for observed and expected deaths
  geom_line() +
  
  # Format x-axis to show one label per month and minor ticks for each week
  scale_x_date(
    date_breaks = "_____", # Question: What are the major and minor breaks?
    date_minor_breaks = "_____",
    date_labels = "%b %Y") +
  
  # Rotating x-axis labels and dropping minor grid lines in the plot panel
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  
  # Question: What are the axis labels?
  xlab("_____") +
  ylab("_____") +
  
  # Dropping the legend title (since there's only one type of presented data)
  labs(col = NULL)

  # Question: What is the title of the graph?
  labs(title = "_____", col = NULL)


#---- Calculating Total Excess Deaths Over Specific Periods ----#

# This section computes the cumulative excess deaths across entire time 
# intervals (not weekly). It's useful for summarizing total excess during 
# specific periods of interest.

all.counts %>%
  excess_model(
    
    # Question: Which dates do we want to exclude from the baseline fitting?
    exclude = _____,
    
    # Instead of specifying a start and stop date, we can specify a period(s) of
    # interest where we want to specifically calculate excess mortality. These
    # periods always have to be included as a list (even if there's only 1).
    intervals = list(
      
      # Full pandemic period
      pandemic.dates,
      
      # Year 1 of the pandemic
      seq(make_date(2020, 03, 10), make_date(2020, 12, 31), by = "day")),
    
    # Question: What would the following be? (Should be same as you set them to above)
    knots.per.year = _____,
    include.trend = _____,
    model = "_____",
    alpha = _____) %>%
  
  # Outputting results as a kable object that we can save or copy/paste into Excel
  kbl()

# Question: What do you notice about the excess mortality using data from 2018
# compared to when you used data from 2010-2018?



#---- Scenario 2: Excess deaths during bad flu season #----

# Let's explore potential excess mortality around the 2014-15 flu season in 
# Michigan. This flu season was moderately severe compared to prior flu seasons
# and caused may high levels of outpatient illness, hospitalization and a 
# relatively high percentage of deaths attributed to pneumonia and influenza.

# Influenza activity began increasing nationally in November and peaked in late 
# December, so we will look at excess mortality informed from the baseline  
# mortality between Jan 1, 2010 to September 30, 2014. 

# We will estimate the expected number of deaths using the baseline data 
# for October 1, 2014 to April 31, 2015.Then we will compare the expected 
# deaths to the observed deaths during that period. 

# Go through the process from Scenario 1 again, but now change the script
# answer Scenario #2 questions.

# Question: How do you read in the full dataset of deaths in Michigan 2010-2020
mort.dat <- _____

# Pretend we didn't have years of data after the flu season 2014-2015.Filter 
# out years after flu season 2014-2015 for this scenario
mort.dat <- mort.dat %>%
  filter(date <= "2015-04-30")

# Current data includes the date of death for each person
# Since we want to run the analysis over weekly counts deaths, 
# we need to figure out which week each death occurred in. 
# We'll assign each date of death to a week of death called isoweek
mort.dat <- _____ %>%
  
  # Question: Which variable are we using isoyear() and isoweek() with?
  mutate(isoweek = _____) %>%
  # Pulling the date of the start of the week each death occurred
  mutate(isodate = _____)) %>%
  
  # Drop the 'date' column so it doesn't confuse any of our other
  # functions later in the analysis
  
  # Question: How do you drop the date column using select()?
  _____ %>%
  
  # Because we're counting by week, the start and end of our dataset don't represent
  # a complete week (1/1/2010 was a Friday, 12/31/2020 was a Thursday), which means
  # that mortality rates for these weeks are going to look lower. We'll drop data
  # from these weeks here, but if you want to analyze a full year of your real
  # data at a weekly level, make sure you have the complete weeks at the beginning
  # and end.
  
  # 2010 to April 2015, how would you adjust the following line? Do we need this line?
  filter(isoweek != "2009-W53-1" & isoweek != "_____")

# To check if we need to filter the data, we can use any of the following approaches
# [insert code here]

#---- Download Population Counts #----

# We want to analyze trends in mortality according to cause of death and county,
# so we need population totals for each of these groups so we can calculate group-
# specific rates of death. These population totals can also change over the time period
# of interest, so we should make sure we have fresh numbers for each
# year to account for those potential differences over time.

# For cause of death, we'll consider the entire population to be at risk, so we
# can use the same population total data from the full analysis. For certain outcomes,
# pregnancy and perinatal deaths, for example, this wouldn't be true so we would
# want to use only female population (or whatever specific population is at risk
# for the outcome under study), but to simplify things here, we'll just just the
# whole population with the assumption that the sex ratio (i.e. the # of males vs.
# females in Michigan) did not change significantly from 2010-2015

# Question: How do you read in the prepared population dataset?
demo.total.in <- _____

# Checking to make sure all years are downloaded
data.frame(
  # Question: Which variable do we use here to mark the beginning and end of the df?
  year = seq(from = _____, na.rm = TRUE), 
             to = _____, na.rm = TRUE),
             _____)) %>%
  mutate(dl = if_else(year %in% demo.total.in$year, "Yes", "No"))

# Question: What years are showing in the mortality data now?
# Answer: 2010-2015

#---- Extrapolate Population Counts #----

# Now we need to convert this table of annual population into a long list of population
# size for each day in our line list. We'll use the 'approx_demographics' function
# from the 'excessmort' package
demo.total <- approx_demographics(
  # Name the original demographic table
  _____,
  # Define the first date that deaths occurred (since we're analyzing at the week
  # level, this is actually the first date of the first week deaths occurred)
  first_day = _____,
  # Define the last date
  last_day = _____,
  # Since we don't have population data for each day or week of each year,
  # we use the population data we do have to extrapolate the time in between. 
  # extrapoloation.type can be constant (i.e. each day of 2015 would have 
  # the same population total until the number is updated with 2016 data), 
  # or can be linear (assume a linear change from year 1 to year 2 and impute the 
  # change along the way). In this case we need to fill in the 
  # gaps so we'll use a linear extrapolation that will assume the population 
  # changed at a fixed rate between the times that we've provided in the 
  # 'demo.total.in' table.
  extrapolation.type = "_____")

#---- Identify Exclusion Period and Aggregate to Weekly Data #----

# We want to evaluate whether there were excess deaths during the flu season, so
# we need to define the exclusion period for our analysis just like for the full
# dataset. Flu season generally runs from Oct-April. We will create a list of 
# dates after which we want to calculate excess. This is basically all of the 
# dates that we don't want to train our expected counts on (flu season 14-15)
flu.dates <- seq.Date(as.Date("_____"), # Question: What is the start date?
                      as.Date("_____"), # Question: What is the end date?
                      by = "_____")

# Finally, we need to aggregate the individual level data into weekly count data 
# and we use compute_counts() to do so which is part of the excessmort package
all.counts <- compute_counts(_____, # df with individual records
                             _____, # df with population data
                             # We want to use weekly counts so we specify date
                             # as the Monday of every week
                             date = "_____")

#---- Calculating Overall Excess Mortality #----

# First, we fit a model to estimate expected weekly mortality and compare it 
# to observed deaths to determine excess mortality over time.

weekly.xs.all <- all.counts %>%
  excess_model(
    # Question: Which dates do we want to exclude from the baseline fitting?
    exclude = _____,
    
    # Define the range of data used to estimate the expected baseline.
    # Here, we use the full available dataset, but this can be adjusted if 
    # needed (e.g., to remove early noisy data or recent incomplete data).
    start = min(.$date),
    
    # Specifying the end of the model. Again, if there are reasons you wouldn't want
    # to use the full dataset (maybe you have low confidence in data completion for
    # recent timepoints) you can do so here.
    end = max(.$date),
    
    # Question: how many knots per year do we want to assign?
    _____ = _____,
    
    # Specify whether you want a linear annual trend included in the prediction.
    # Not recommended if you have 5 or fewer years of baseline data.
    _____ = _____,
    
    # Question: What type of model should we use?
    _____ = _____,
    
    # Question: What significance level if we want 95% confidence interval?
    _____ = _____)

# Convert the model output (a list) into a more user-friendly data frame format.
# We're only keeping key columns: date, observed deaths, expected deaths, 
# and the standard error of the expected counts
weekly.xs.all <- bind_cols(weekly.xs.all[1:4]) %>%
  
  # Calculate the 95% confidence interval bounds for expected deaths using SE
  # Note: expected SE is on the log scale, so we exponentiate it before 
  # applying the normal quantile.
  
  # Question: What do you set qnorm to for this scenario?
  mutate(exp.l95 = expected + qnorm(0.025) * exp(log_expected_se),
         exp.u95 = expected + qnorm(0.975) * exp(log_expected_se))

#---- Preparing Data for Plotting ----#

# Restructuring for plotting . ggplot likes data formatted in a long format, 
# (one column for values, one for category label) so we're stacking the observed
# and expected counts in a single column and adding a new label called 'class'
# to describe whether that number is an observed or expected count.

# Question: Which data frame are we plotting?
weekly.all.plotdat <- _____ %>%
  
  # Question: Which dates will we include in the plot?
  filter(_____ > make_date(_____)) %>%
  
  # Convert 'observed' and 'expected' columns into a single 'value' column,
  # with a new 'class' column labeling the type of value (Observed vs Expected).
  pivot_longer(cols = _____,
               names_to = "_____",
               values_to = "_____") %>%
  
  # Renaming and reordering 'class' to make the legend of our plot look nicer
  mutate(class = factor(_____,
                        
                        # Question: What will the labels be in this case?
                        labels = c(_____)))

#---- Plotting Observed vs Expected Weekly Deaths ----#

# Plotting observed vs expected
# Questions: What dataframe are we plotting? What is the x-value?
ggplot(_____, aes(_____)) +
  
  # Draw lines for observed and expected deaths
  geom_line() +
  
  # Format x-axis to show one label per month and minor ticks for each week
  scale_x_date(
    _____, # Question: What are the major and minor breaks?
    _____,
    _____) +
  
  # Rotating x-axis labels and dropping minor grid lines in the plot panel
  theme(axis.text.x = element_text(_____, _____),
        panel.grid.minor = element_blank()) +
  
  # Question: What are the axis labels?
  _____ +
  _____ +
  
  # Dropping the legend title (since there's only one type of presented data)
  labs(col = NULL) +
  
  # Question: What is the title of the graph?
  labs(_____)


#---- Calculating Total Excess Deaths Over Specific Periods ----#

# This section computes the cumulative excess deaths across entire time 
# intervals (not weekly). It's useful for summarizing total excess during 
# specific periods of interest.

all.counts %>%
  _____(
    
    # Question: Which dates do we want to exclude from the baseline fitting?
    exclude = _____,
    
    # Instead of specifying a start and stop date, we can specify a period(s) of
    # interest where we want to specifically calculate excess mortality. These
    # periods always have to be included as a list (even if there's only 1).
    intervals = list(
      
      # Question: Full flu season
      _____,
      
      # Flu season
      seq(make_date(2014, 10, 01), make_date(2015, 04, 30), by = "day")),
    
    # Question: What would the following be? (Should be same as you set them to above)
    _____, # Question: knots?
    _____, # Question: trend?
    _____, # Question: model type?
    _____) %>% # Question: alpha level?
  
  # Outputting results as a kable object that we can save or copy/paste into Excel
  kbl()

# Question: What do you notice about the excess mortality during the 2014-15 
# flu season?
# Answer: The expected deaths are lower than the observed deaths throughout
# flu season. Most notable the observed deaths are far higher during Dec-Jan
# which does align with the peak of flu season in Michigan of this year.