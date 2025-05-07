# Quick generation of final tables and figures for (1) overall analysis of excess
# deaths in 2020, (2) subgroup analysis by cause of death, (3) subgroup analysis
# of age, (4) subgroup analysis of race

pkgs <- c("tidyverse", "tidycensus", "ISOweek", "excessmort", "openxlsx", "kableExtra")
lapply(pkgs, library, character.only = TRUE)

# Setting up output directory
outdir <- "/Users/kyleaune/Library/CloudStorage/OneDrive-SharedLibraries-JohnsHopkins/SORT - CSTE Training/Draft Materials/03_Guided Practice/Draft Output/"

dat <- readRDS("linelist_2010-2020_mi.rda") %>%
  # Assigning each death to the week it occurred in
  mutate(isoweek = paste0(isoyear(date), "-W", sprintf("%02d", isoweek(date)),"-1")) %>%
  # Listing the date of the start of the week each death occurred
  mutate(isodate = ISOweek2date(isoweek)) %>%
  select(-date)

# Read in demograhpics
demo.total.in <- read_csv("demo_by_year_2010-2020_MI.csv")
demo.co.in <- read_csv("demo_by_year_by_county_2010-2020_MI.csv")
demo.race.in <- read_csv("demo_by_year_by_race_2010-2020_MI.csv")
demo.age.in <- read_csv("demo_by_year_by_age_2010-2020_MI.csv")

# Approximate total annual demographics
demo.total <- approx_demographics(demo.total.in,
                                  first_day = min(dat$isodate),
                                  last_day = max(dat$isodate),
                                  extrapolation.type = "linear")

# Aggregating line list to weekly count data
all.counts <- compute_counts(dat,
                             demo = demo.total,
                             date = "isodate")

pandemic.dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")

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
    frequency = 52,
    alpha = 0.05)

weekly.xs.all <- bind_cols(weekly.xs.all[1:4]) %>%
  # Calculating 95% confidence intervals of expected deaths
  mutate(exp.l95 = expected + qnorm(0.025) * exp(log_expected_se),
         exp.u95 = expected + qnorm(0.975) * exp(log_expected_se))

weekly.all.plotdat <- weekly.xs.all %>%
  # Dropping last row (incomplete week)
  filter(date < make_date(2020, 12, 27)) %>%
  # Setting starting time of plot
  filter(date > make_date(2020, 01, 01)) %>%
  # Stacking observed and expected to make ggplot creation easier
  pivot_longer(cols = observed:expected,
               names_to = "class",
               values_to = "value") %>%
  # Renaming and reordering to make the legend of our plot look nicer
  mutate(class = factor(class, levels = c("observed", "expected"),
                        labels = c("Observed", "Expected")))

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
ggsave(paste0(outdir, "overall_excess.png"), height = 5, width = 8, dpi = 300, units = "in")

# Calculating total excess by period of time
all.counts %>%
  excess_model(
    exclude = pandemic.dates,
    # Instead of specifying a start and stop date, we can specify a period(s) of
    # interest where we want to specifically calculate excess mortality. These
    # periods always have to be included as a list (even if there's only 1).
    intervals = list(
      # Full pandemic period
      pandemic.dates),
    knots.per.year = 12,
    include.trend = FALSE,
    model = "quasipoisson",
    alpha = 0.05) %>%
  # Adding labels for the period results
  mutate(period = c("COVID-19")) %>%
  # Moving the label column to the left of the table
  relocate(period) %>%
  # Outputting results as a kable object that we can save or copy/paste into Excel
  write.xlsx(paste0(outdir, "overall_excess.xlsx"), asTable = TRUE)


  