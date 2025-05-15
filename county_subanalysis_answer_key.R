#----------------------------------------------------------------#
#               2025 CSTE Applied Trend Analysis in R            #
#   Subgroup Analysis Strategies - Guided Practice Answer Key    #
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
mort.dat <- readRDS("linelist_2010-2020_mi.rda") %>%
  mutate(isoweek = paste0(isoyear(date), "-W", sprintf("%02d", isoweek(date)),"-1")) %>%
  mutate(isodate = ISOweek2date(isoweek)) %>%
  select(-date) %>%
  filter(isoweek != "2009-W53-1" & isoweek != "2020-W53-1")

# Read in full demographic data
demo.total.in <- read_csv("demo_by_year_2010-2020_MI.csv")

# Create a long list of daily population by county
demo.total <- approx_demographics(
  demo = demo.total.in,
  first_day = min(mort.dat$isodate),
  last_day = max(mort.dat$isodate),
  extrapolation.type = "linear")

# Define the pandemic period (when we don't want to calculate expected deaths)
pandemic.dates <- seq.Date(as.Date("2020-03-10"), as.Date("2020-12-31"), by = "day")


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