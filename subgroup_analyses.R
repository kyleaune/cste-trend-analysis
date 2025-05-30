#----------------------------------------------------------------#
#               2025 CSTE Applied Trend Analysis in R            #
# Subgroup Analysis Strategies - Demonstration & Guided Practice #
# Authors: Johns Hopkins Surveillance and Outbreak Response Team #
#----------------------------------------------------------------#

#---- Setup #----

# List of package names as characters
pkgs <- c("tidyverse", "ISOweek", "excessmort", "ggpubr", "openxlsx")

# Download and install any packages that are missing
install.packages(setdiff(pkgs, installed.packages()), install.packages())

# Load packages
lapply(pkgs, library, character.only = TRUE)

# Read in mortality data set
mort.dat <- read_csv("linelist_2010-2020_mi.csv") %>%
  # Since we want to run the analysis over weekly counts of observed vs. expected
  # deaths, we need to figure out which week each death occurred in. We'll assign
  # each date of death to a week of death
  mutate(isoweek = paste0(isoyear(date), "-W", sprintf("%02d", isoweek(date)),"-1")) %>%
  # And then we'll pull the actual date of the first day of that week
  mutate(isodate = ISOweek2date(isoweek)) %>%
  # Next, we'll drop the 'date' column so it doesn't confuse any of our other
  # functions later in the analysis
  select(-date) %>%
  # Because we're counting by week, the start and end of our dataset don't represent
  # a complete week (1/1/2010 was a Friday, 12/31/2020 was a Thursday), which means
  # that mortality rates for these weeks are going to look lower. We'll drop data
  # from these weeks here, but if you want to analyze a full year of your real
  # data at a weekly level, make sure you have the complete weeks at the beginning
  # and end.
  filter(isoweek != "2009-W53-1" & isoweek != "2020-W53-1")

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

# Read in full demographic data
demo.total.in <- read_csv("demo_by_year_2010-2020_MI.csv")

# Now we need to convert this table of annual population into a long list of population
# size for each day in our line list. We'll use the 'approx_demographics' function
# from the 'excessmort' pacakge.
demo.total <- approx_demographics(
  # Name the original demographic table
  demo = demo.total.in,
  # Define the first date that deaths occurred (since we're analyzing at the week
  # level, this is actually the first date of the first week deaths occurred)
  first_day = min(mort.dat$isodate),
  # Define the last date
  last_day = max(mort.dat$isodate),
  # Since we have decennial census totals for 2010, but no information again until
  # 2015, we need to extrapolate the time in between. We could frequently assume
  # counts to remain the same from year to year (i.e. each day of 2015 would have
  # the same population total until the number is updated with 2016 data), but in
  # this case we need to fill in the gaps so we'll use a linear extrapolation that
  # will assume the population changed at a fixed rate between the times that we've
  # provided in the 'demo.total.in' table.
  extrapolation.type = "linear")

# We want to evaluate whether there were excess deaths during the pandemic, so
# we need to define the exclusion period for our analysis just like for the full
# dataset. The first COVID-19 case was reported in Michigan on March 10, 2020
pandemic.dates <- seq.Date(as.Date("2020-03-10"), as.Date("2020-12-31"), by = "day")


#---- Sub-analysis by Cause of Death #----

# The data provided by CDC Wonder lists 19 causes of death, but the frequency of
# each cause varies pretty substantially - there are lots of cardiovascular and
# cancer deaths, but not many deaths due to causes like ear disease. Let's combine
# some categories to make our analysis and comparison more useful and intuitive
# to interpret.

# Let's take a look at the causes that are experienced to see how we want to group.
# things together
sort(table(mort.dat$cause) / nrow(mort.dat) * 100, decreasing = TRUE)
  # Let's move pregnancy and perinatal together, then group anything under 1% as 'other'

# Further categorizing cause of death
mort.dat <- mort.dat %>%
  # We're going to use the 'case_when' function which is basically like a lot of
  # nested 'if, else' statements
  mutate(
    cause.cat = case_when(cause %in% c("Pregnancy (O00-O99)", "Perinatal (P00-P96)") ~
                            "Pregnancy & Perinatal (O00-P96)",
                          cause %in% c("Blood & Immune Disorders (D50-D89)",
                                       "Congenital Malformation (Q00-Q99)",
                                       "Ear Disease (H60-H93)", "Eye Disease (H00-H57)",
                                       "Musculoskeletal (M00-M99)", "Skin Disease (L00-L98)",
                                       "Abnormal (R00-R99)") ~ "Other",
                          # We can now set everything else to stay the same
                          .default = cause))

# Now we need to count how many deaths occurred for each cause, in each week
cause.counts <- compute_counts(
  # Specify the line list
  dat = mort.dat,
  # Specify the daily demographic list
  demo = demo.total,
  # Specify the name of the column with date of death at the unit we want to analyze
  date = "isodate",
  # Specify the name of the column with cause of death categories
  by = "cause.cat")

# Next, we're going to run an excess calculation model on each of the subcategories.
# We could do this the hard way, by manually filtering the 'cause.counts' table
# by each cause and running 14 separate models, or we could use programming loops
# to accomplish the same goal.

# First, let's split 'cause.counts' by cause of death. We'll use the 'split'
# function which will divide the full table by cause and store each as a data frame
# in a list.
cause.counts.ls <- split(cause.counts,
                         # Define the factor (f) we want to split by
                         f = cause.counts$cause.cat)

# Now we're going to use the 'lapply' function to apply the same process to each
# item in the list we just created.
weekly.xs.cause <- lapply(
  cause.counts.ls, 
  # We're going to define a new function that operates on
  # each item in 'cause.counts.ls', which is represented
  # by the variable x
  function(x) {
    # Wrapping everything in 'return()' means that the results of the function
    # get returned and stored as an item in our output list (called 'weekly.xs.cause')
    return(
      x %>%
        # We're running the same model as for the full dataset, only in each case
        # here, we've filtered to only include one specific cause of death at a time
        excess_model(exclude = pandemic.dates,
                     start = min(.$date),
                     end = max(.$date),
                     knots.per.year = 12,
                     include.trend = TRUE,
                     frequency = 52,
                     model = "quasipoisson",
                     alpha = 0.05,
                     keep.counts = TRUE)
      )
    })

# Just like before, this results in a pretty long output, so let's restructure. 
# We want to have a data frame where each row is a week and we have information for
# observed and expected death counts and rates and the confidence of these estimates.
# Since we have a list of results for each cause, we'll use 'lapply' again to
# do this restructuring.
weekly.cause.ts <- lapply(
  weekly.xs.cause,
  function(x) {
    # The output of the 'excess_model' function is a list of results for each cause,
    # but we only want the first few.
    x <- bind_cols(x[c("date", "observed", "expected", "fitted", "log_expected_se")]) %>%
        ## 'date' is the start of the week, 'observed' and 'expected' are counts
        ## of deaths that are either observed during the COVID-19 pandemic or
        ## expected based on 2010-2019 data, 'fitted' is the relative difference
        ## between observed and expected (f = (observed - expected) / expected),
        ## and 'se' is the standard error of that relative difference.
      # Let's use the standard error (and some algebra) to calculate confidence
      # intervals of the expect count of weekly deaths
      mutate(exp.l95 = exp(log(expected) - qnorm(1-(.05/2)) * log_expected_se),
             exp.u95 = exp(log(expected) + qnorm(1-(.05/2)) * log_expected_se))
  }
)

# Now let's plot a time series of the weekly counts of observed vs. expected
# # (and 95% confidence intervals) mortality. This time we're going to loop over
# two things though - (1) the weekly excess counts split up by cause of death and
# (2) a list of the causes of death so we can include descriptive titles in the
# graph. Because we're using more than one list, we have to adapt our code to use
# 'mapply' (for multivariate apply) instead of 'lapply' like above. It's structured
# a little different, but the idea is generally the same. This time though, we can
# define more than one variable like you'll see below. Also, since we're plotting
# observed vs. expected deaths, this setup doesn't make much sense for visualizing
# COVID-19 caused deaths (ICD-10 chapter U codes), since there's no historical data
# to determine what would be "expected" (in reality, we would expect zero deaths).
# So we'll plot all the other, non-COVID-19 causes here, and plot observed COVID-19
# deaths by themselves.

# Plotting observed vs expected deaths from 1/1/2020 - 12/31/2020
xs.cause.plots <- mapply(
  # We're going to define a function with two arguments, x and y, this time
  # We'll use 'x' to represent the 'weekly.cause.ts' element and 'y' to represent
  # the cause of death used in titling and labeling the figure
  function(x, y) {
    # The time series data includes the entire time period but we only want to
    # plot the year 2020
    x <- x %>%
      filter(date > make_date(2020, 01, 01)) %>%
      # We're going to use 'ggplot' to make the figures, and it prefers long data
      # structures over wide
      pivot_longer(cols = c(observed:expected),
                   names_to = "class",
                   values_to = "value") %>%
      # Reordering and fixing labels for observed and expected categories
      mutate(class = factor(class, levels = c("observed", "expected"),
                            labels = c("Observed", "Expected")))
    
    # Plotting observed and expected (+95% CI) deaths
    return(
      # This line might look a little confusing because ggplot needs data for the
      # x and y axes, but 'x' is the data we're plotting and we're specifying inside
      # the aes() call what goes along the x- and y-axis.
      ggplot(x, aes(x = date, y = value, col = class)) +
        geom_line() +
        geom_ribbon(data = x %>% filter(class == "Expected"),
                    aes(ymin = exp.l95, ymax = exp.u95),
                    fill = NA, lty = 2) +
        scale_x_date(
          date_breaks = "1 month",
          date_minor_breaks = "1 week",
          date_labels = "%b %Y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank()) +
        xlab("Date") +
        ylab("Weekly Death Count") +
        labs(col = NULL,
             title = "Excess Mortality 2020",
             subtitle = paste(y, "Cause of Death", sep = " "))
    )
  },
  # Now we'll specify what 'x' and 'y' represent in the function above
  # 'x' will be the cause time series that we cleaned up, but we'll skip COVID-19
  # deaths. Since those are stored as the 14th element in the 'weekly.cause.ts'
  # list, we can drop it
  x = weekly.cause.ts[-14],
  # 'y' is used to generate plot titles and is the names of the elements of the
  # 'weekly.cause.ts' list (which are the names of the cause of death categories).
  # We still don't want to include COVID-19 deaths here, so we'll drop them.
  y = names(weekly.cause.ts[-14]),
  # Finally, we have to ask R to not wrap the results up in a list for us (the default)
  SIMPLIFY = FALSE
)

# Now let's plot just the observed COVID-19 deaths (without including an expected
# curve).
# We need to reshape the dataframe before we can plot it
covid.plotdat <- weekly.cause.ts[[14]] %>%
  filter(date > make_date(2020, 01, 01))

covid.plot <- ggplot(covid.plotdat, aes(x = date, y = observed, lty = "Observed")) +
  geom_line(col = "#F8766D") +
  scale_x_date(
    date_breaks = "1 month",
    date_minor_breaks = "1 week",
    date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  xlab("Date") +
  ylab("Weekly Death Count") +
  labs(col = NULL,
       title = "Observed Mortality 2020",
       subtitle = paste(names(weekly.cause.ts[14]), "Cause of Death", sep = " "))

# Let's add the plot of COVID-19 deaths back to our full list of plots by cause
# of death.
xs.cause.plots <- c(xs.cause.plots, list(covid.plot))
# Now let's give each plot a name (that we can use to easily refer to each plot
# or use in filenames when saving).
names(xs.cause.plots) <- names(weekly.cause.ts)

# Now that we have nice figures, let's save high-quality versions so we can share
# them in reports and presentations. With a single figure, we could write one
# line of code, but since we have lots of them here, let's use another 'mapply'
# loop to make things more efficient.
mapply(
  # We're going to define another function with two variables. 'x' will be the
  # figure we created and saved as 'xs.cause.plots' and 'y' will be the filenames
  # we want to save as
  function(x, y) {
    ggsave(y, plot = x,
           # We can also specify what we want to figure to look like and its resolution
           # We'll make a 6.5x3 inch figure at 300 dots-per-inch (agood presentation
           # quality resolution)
           width = 6.5, height = 3, units = "in", dpi = 300)
  },
  x = xs.cause.plots, 
  # We're going to use the names of the 'xs.cause.plots' figures, but some of them
  # have '&' symbols and many of them have spaces which can be troublesome for
  # file naming sometimes, so we'll replace all of those with dashes and underscores
  # using the 'gsub' function. Since we want to replace two different characters,
  # we'll need to nest one 'gsub' within another.
  y = paste0("excess_",
             gsub(" ", "_",
                  gsub(" & ", "-", names(xs.cause.plots))),
             "_deaths.png"))


# Now that we have a plot, let's make a table of observed, expected, and excess
# counts and rates for each cause. Since we're looping over two lists, we'll use
# 'mapply' again where 'x' will refer to the weekly count data for each cause and
# 'y' will refer to the names of the causes.
cause.table <- mapply(function(x, y) {
  return(x %>%
           # We're going to run a new model, but this time we won't specify start
           # and end dates that we want to analyze. We'll still specify that we
           # want to exclude the pandemic from calculating our baseline expected rate.
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           # Calculating standard error for confidence interval
           mutate(se = sd / sqrt(observed)) %>%
           # Calculating excess confidence interval
           mutate(excess.l95 = excess - qnorm(1-(.05/2)) * se,
                  excess.u95 = excess + qnorm(1-(.05/2)) * se) %>%
           # Calculating mortality ratio (observed / expected) & 95% CI
           mutate(mr = observed / expected,
                  mr.l95 = observed / (expected + qnorm(1-(.05/2)) * se),
                  mr.u95 = observed / (expected - qnorm(1-(.05/2)) * se)) %>%
           # Giving each row the name of the cause of death it represents
           mutate(cause = y) %>%
           # Moving the cause column all the way to the left
           relocate(cause) %>%
           # Calculating the excess rate
           mutate(xs_rate = obs_death_rate - exp_death_rate))
  }, 
  # Just like above, we'll specify what 'x' and 'y' stand for when looping though
  x = cause.counts.ls,
  y = names(cause.counts.ls),
  SIMPLIFY = FALSE) %>%
  # The output is a list of data frames (each with a single row for that cause
  # of death), so we'll stack them all together into a single dataframe
  bind_rows()

 # We can save this table as an Excel file so we can share it or reformat it
write.xlsx(cause.table, "excess_deaths_by_cause.xlsx", asTable = TRUE)


#---- Sub-analysis by County #----

# The full dataset 'mort.dat' is also recorded at the county level, so let's see
# if there were any big differences in 2020 excess deaths by county

# For county-specific rates of excess mortality, we'll use the same type of setup
# for demographic data, but this time we have a row for every county's population
# in every year.

# Read in county-level demographic data
demo.county.in <- read_csv("demo_by_year_by_county_2010-2020_MI.csv")

# But since there are 83 counties in Michigan, let's make things a little easier
# on ourselves and select four counties to look at. How about two urban and two
# rural. Let's look at Wayne County (Detroit), Kent County (Grand Rapids), 
# Marquette (in the Upper Pensinsula), and Lapeer (outside Detroit)

# Filter to these counties
mort.dat.co <- mort.dat %>%
  filter(county %in% c("Wayne County, MI", "Kent County, MI",
                       "Marquette County, MI", "Lapeer County, MI"))

# Let's filter the demographics too
demo.county.in.sub <- demo.county.in %>%
  filter(county %in% c("Wayne County, MI", "Kent County, MI",
                       "Marquette County, MI", "Lapeer County, MI"))

# We need to take that table of annual population by county and create a long
# dataframe with daily population.
demo.county <- approx_demographics(demo.county.in.sub,
                                   first_day = min(mort.dat$isodate),
                                   last_day = max(mort.dat$isodate),
                                   by = "county",
                                   extrapolation.type = "linear")

# Count how many deaths occurred in each county, each week
county.counts <- compute_counts(
  dat = mort.dat.co,
  demo = demo.county,
  date = "isodate",
  by = "county")

# Split the mortality data by county into a list of dataframes
county.counts.ls <- split(county.counts, f = county.counts$county)

# Run the excess model function on each item in the list
weekly.xs.county <- lapply(
  county.counts.ls, 
  function(x) {
    return(
      x %>%
        excess_model(exclude = pandemic.dates,
                     start = min(.$date),
                     end = max(.$date),
                     knots.per.year = 12,
                     include.trend = FALSE,
                     frequency = 52,
                     model = "quasipoisson",
                     alpha = 0.05,
                     keep.counts = TRUE)
    )
  })

# Restructure the results for plotting
weekly.county.ts <- lapply(
  weekly.xs.county,
  function(x) {
    x <- bind_cols(x[c("date", "observed", "expected", "fitted", "log_expected_se")]) %>%
      mutate(exp.l95 = exp(log(expected) - qnorm(1-(.05/2)) * log_expected_se),
             exp.u95 = exp(log(expected) + qnorm(1-(.05/2)) * log_expected_se))
  }
)

# Plot excess death by county
xs.county.plots <- mapply(
  function(x, y) {
    x <- x %>%
      filter(date > make_date(2020, 01, 01)) %>%
      pivot_longer(cols = c(observed:expected),
                   names_to = "class",
                   values_to = "value") %>%
      mutate(class = factor(class, levels = c("observed", "expected"),
                            labels = c("Observed", "Expected")))
    
    return(
      ggplot(x, aes(x = date, y = value, col = class)) +
        geom_line() +
        geom_ribbon(data = x %>% filter(class == "Expected"),
                    aes(ymin = exp.l95, ymax = exp.u95),
                    fill = NA, lty = 2) +
        scale_x_date(
          date_breaks = "1 month",
          date_minor_breaks = "1 week",
          date_labels = "%b %Y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank()) +
        xlab("Date") +
        ylab("Weekly Death Count") +
        labs(col = NULL,
             title = "Excess Mortality 2020",
             subtitle = y)
    )
  },
  x = weekly.county.ts,
  y = names(weekly.county.ts),
  SIMPLIFY = FALSE
)

# Save the figures for each county
mapply(
  function(x, y) {
    ggsave(y, plot = x,
           width = 6.5, height = 3, units = "in", dpi = 300)
  },
  x = xs.cause.plots, 
  y = paste0("excess_",
             gsub(" ", "_",
                  gsub(" & ", "-", names(xs.cause.plots))),
             "_deaths.png"))

# Create a table of statistics for each county
county.table <- mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(se = sd / sqrt(observed)) %>%
           mutate(excess.l95 = excess - qnorm(1-(.05/2)) * se,
                  excess.u95 = excess + qnorm(1-(.05/2)) * se) %>%
           mutate(mr = observed / expected,
                  mr.l95 = observed / (expected + qnorm(1-(.05/2)) * se),
                  mr.u95 = observed / (expected - qnorm(1-(.05/2)) * se)) %>%
           mutate(cause = y) %>%
           relocate(cause) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
  }, 
  x = county.counts.ls,
  y = names(county.counts.ls),
  SIMPLIFY = FALSE) %>%
  bind_rows()

# Save the table as an Excel file
write.xlsx(county.table, "excess_deaths_by_county.xlsx", asTable = TRUE)
