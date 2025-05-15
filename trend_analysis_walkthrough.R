#-------------------------------------------------------------------------#
#               2025 CSTE Applied Trend Analysis in R                     #
#                   Trend Analysis - Demonstration                        #
# Authors: Johns Hopkins Surveillance, Outbreak, and Response Team        #
#-------------------------------------------------------------------------#

#---- Calculating Overall Excess Mortality #----

# First, we fit a model to estimate expected weekly mortality and compare it 
# to observed deaths to determine excess mortality over time.

weekly.xs.all <- all.counts %>%
  excess_model(
    # Exclude dates during the COVID-19 pandemic so they don't influence the
    # baseline model
    exclude = pandemic.dates,
    
    # Define the range of data used to estimate the expected baseline.
    # Here, we use the full available dataset, but this can be adjusted if 
    # needed (e.g., to remove early noisy data or recent incomplete data).
    start = min(.$date),
    
    # Specifying the end of the model. Again, if there are reasons you wouldn't want
    # to use the full dataset (maybe you have low confidence in data completion for
    # recent timepoints) you can do so here.
    end = max(.$date),
    
    # Control the flexibility of the baseline model by setting the number of 
    # spline knots per year. More knots allow the model to capture seasonal 
    # variation better. 12 allows for monthly changes in baseline prediction model
    knots.per.year = 12,
    
    # Specify whether you want a linear annual trend included in the prediction.
    # Not recommended if you have 5 or fewer years of baseline data.
    include.trend = FALSE,
    
    # Specify the model type (quasi-Poisson helps account for overdispersion 
    # in count data).
    model = "quasipoisson",
    
    # Set the significance level for confidence intervals.
    alpha = 0.05)



# Convert the model output (a list) into a more user-friendly dataframe format.
# We're only keeping key columns: date, observed deaths, expected deaths, 
# and the standard error of the expected counts
weekly.xs.all <- bind_cols(weekly.xs.all[1:4]) %>%
  
  # Calculating 95% confidence intervals of expected deaths
  # Calculate the 95% confidence interval bounds for expected deaths using SE
  # Note: expected SE is on the log scale, so we exponentiate it before 
  # applying the normal quantile.
  mutate(exp.l95 = expected + qnorm(0.025) * exp(log_expected_se),
         exp.u95 = expected + qnorm(0.975) * exp(log_expected_se))

#---- Preparing Data for Plotting ----#

# Restructuring for plotting . ggplot likes data formatted in a long format, 
# (one column for values, one for category label) so we're stacking the observed
# and expected counts in a single column and adding a new label called 'class'
# to describe whether that number is an observed or expected count.

weekly.all.plotdat <- weekly.xs.all %>%
  
  # Filter to plot only data after Jan 1, 2020
  filter(date > make_date(2020, 01, 01)) %>%
  
  # Convert 'observed' and 'expected' columns into a single 'value' column,
  # with a new 'class' column labeling the type of value (Observed vs Expected).
  pivot_longer(cols = observed:expected,
               names_to = "class",
               values_to = "value") %>%
  
  # Renaming and reordering 'class' to make the legend of our plot look nicer
  mutate(class = factor(class, levels = c("observed", "expected"),
                        labels = c("Observed", "Expected")))

#---- Plotting Observed vs Expected Weekly Deaths ----#

# Plotting observed vs expected
ggplot(weekly.all.plotdat, aes(x = date, y = value, col = class)) +
  
  # Draw lines for observed and expected deaths
  geom_line() +
  
  # Format x-axis to show one label per month and minor ticks for each week
  scale_x_date(
    date_breaks = "1 month",
    date_minor_breaks = "1 week",
    date_labels = "%b %Y") +
  
  # Rotating x-axis labels and dropping minor grid lines in the plot panel
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  
  # Label axes
  xlab("Date") +
  ylab("Weekly Death Count") +
  
  # Dropping the legend title (since there's only one type of presented data)
  labs(col = NULL)

#---- Calculating Total Excess Deaths Over Specific Periods ----#

# This section computes the cumulative excess deaths across entire time 
# intervals (not weekly). It's useful for summarizing total excess during 
# specific periods of interest.

all.counts %>%
  excess_model(
    
    # Again exclude pandemic dates from the baseline fitting
    exclude = pandemic.dates,
    
    # Instead of specifying a start and stop date, we can specify a period(s) of
    # interest where we want to specifically calculate excess mortality. These
    # periods always have to be included as a list (even if there's only 1).
    intervals = list(
      
      # Full pandemic period
      pandemic.dates,
      
      # Year 1 of the pandemic
      seq(make_date(2020, 03, 10), make_date(2020, 12, 31), by = "day")),
    
    # Same model settings as before
    knots.per.year = 12,
    include.trend = FALSE,
    model = "quasipoisson",
    alpha = 0.05) %>%
  # Outputting results as a kable object that we can save or copy/paste into Excel
  kbl()


