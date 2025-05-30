# Quick generation of final tables and figures for (1) overall analysis of excess
# deaths in 2020, (2) subgroup analysis by cause of death, (3) subgroup analysis
# of age, (4) subgroup analysis of race

pkgs <- c("tidyverse", "tidycensus", "ISOweek", "excessmort", "openxlsx", "ggpubr",
          "tigris", "sf", "tmap")
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

pandemic.dates <- seq.Date(as.Date("2020-03-10"), as.Date("2020-12-31"), by = "day")

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
  mutate(exp.l95 = exp(log(expected) - qnorm(1-(.05/2)) * log_expected_se),
         exp.u95 = exp(log(expected) + qnorm(1-(.05/2)) * log_expected_se))

weekly.all.plotdat <- weekly.xs.all %>%
  # Dropping last row (incomplete week)
  filter(date < make_date(2020, 12, 27)) %>%
  # Setting starting time of plot
  filter(date > make_date(2019, 01, 01)) %>%
  # Stacking observed and expected to make ggplot creation easier
  pivot_longer(cols = observed:expected,
               names_to = "class",
               values_to = "value") %>%
  # Renaming and reordering to make the legend of our plot look nicer
  mutate(class = factor(class, levels = c("observed", "expected"),
                        labels = c("Observed", "Expected")))

ggplot(weekly.all.plotdat, aes(x = date, y = value, col = class)) +
  geom_line() +
  geom_ribbon(data = weekly.all.plotdat %>% filter(class == "Expected"),
              aes(ymin = exp.l95, ymax = exp.u95),
              fill = NA, lty = 2) +
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
  labs(col = NULL,
       title = "Excess Mortality 2020")
ggsave(paste0(outdir, "overall_excess.png"), height = 2.5, width = 6.5, dpi = 300, units = "in")

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
  # Calculating standard error for confidence interval
  mutate(se = sd / sqrt(observed)) %>%
  # Calculating excess confidence interval
  mutate(excess.l95 = excess - qnorm(1-(.05/2)) * se,
         excess.u95 = excess + qnorm(1-(.05/2)) * se) %>%
  # Calculate mortality ratios (& 95% CIs)
  mutate(mr = observed / expected,
         mr.l95 = observed / (expected + qnorm(1-(.05/2)) * se),
         mr.u95 = observed / (expected - qnorm(1-(.05/2)) * se)) %>%
  # Adding labels for the period results
  mutate(period = c("COVID-19")) %>%
  # Moving the label column to the left of the table
  relocate(period) %>%
  # Outputting results as a kable object that we can save or copy/paste into Excel
  write.xlsx(paste0(outdir, "overall_excess.xlsx"), asTable = TRUE)


#---- Subanalysis by Race #----

# Approximate annual race demographics
demo.race <- approx_demographics(demo.race.in,
                                 first_day = min(dat$isodate),
                                 last_day = max(dat$isodate),
                                 extrapolation.type = "linear")

# Counting deaths by week per race group
race.counts <- compute_counts(dat,
                              demo = demo.race,
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
                   frequency = 52,
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
    mutate(exp.l95 = exp(log(expected) - qnorm(1-(.05/2)) * log_expected_se),
           exp.u95 = exp(log(expected) + qnorm(1-(.05/2)) * log_expected_se))
  
  return(x %>%
           # Setting starting time of plot
           filter(date > make_date(2020, 01, 01) &
                    date < make_date(2020, 12, 27)) %>%
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
           subtitle = paste(y, "Race", sep = " "))
  )
}, x = weekly.race.plotdat, y = names(weekly.race.plotdat), SIMPLIFY = FALSE)

ggarrange(xs.race.plots[[1]], xs.race.plots[[2]], xs.race.plots[[3]], xs.race.plots[[4]],
          nrow = 4)
ggsave(paste0(outdir, "excess_byrace.png"), height = 9, width = 6.5, dpi = 300, units = "in")

# Calculating overall excess during pandemic by race group
mapply(function(x, y) {
  return(x %>%
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
           # Calculate mortality ratios (& 95% CIs)
           mutate(mr = observed / expected,
                  mr.l95 = observed / (expected + qnorm(1-(.05/2)) * se),
                  mr.u95 = observed / (expected - qnorm(1-(.05/2)) * se)) %>%
           mutate(race = y) %>%
           relocate(race) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = race.counts.ls, y = names(race.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  write.xlsx(paste0(outdir, "excess_byrace.xlsx"), asTable = TRUE)


#---- Subanalysis by Age #----

# Approximate annual age demographics
demo.age <- approx_demographics(demo.age.in,
                                first_day = min(dat$isodate),
                                last_day = max(dat$isodate),
                                extrapolation.type = "linear")
# Counting deaths by week per age group
age.counts <- compute_counts(dat,
                             demo = demo.age,
                             date = "isodate",
                             by = "age") %>%
  # Drop <18 year olds
  filter(age != "<18")


# Fitting model to calculate excess mortality
age.counts.ls <- split(age.counts, f = age.counts$age)
weekly.xs.age <- lapply(age.counts.ls, function(x) {
  return(
    x %>%
      excess_model(exclude = pandemic.dates,
                   start = min(.$date),
                   end = max(.$date),
                   knots.per.year = 12,
                   include.trend = FALSE,
                   frequency = 52,
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
    mutate(exp.l95 = exp(log(expected) - qnorm(1-(.05/2)) * log_expected_se),
           exp.u95 = exp(log(expected) + qnorm(1-(.05/2)) * log_expected_se))
  
  return(x %>%
           # Setting starting time of plot
           filter(date > make_date(2020, 01, 01) &
                    date < make_date(2020, 12, 27)) %>%
           # Stacking observed and expected to make ggplot creation easier
           pivot_longer(cols = observed:expected,
                        names_to = "class",
                        values_to = "value") %>%
           mutate(class = factor(class, levels = c("observed", "expected"),
                                 labels = c("Observed", "Expected"))))
})

# Plotting observed vs expected
xs.age.plots <- mapply(function(x, y) {
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
           subtitle = paste(y, "Years Old", sep = " "))
  )
}, x = weekly.age.plotdat, y = names(weekly.age.plotdat), SIMPLIFY = FALSE)

ggarrange(xs.age.plots[[1]], xs.age.plots[[2]], nrow = 2)
ggsave(paste0(outdir, "excess_byage.png"), height = 4.5, width = 6.5, dpi = 300, units = "in")

# Calculating overall excess during pandemic by race group
mapply(function(x, y) {
  return(x %>%
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
           # Calculate mortality ratios (& 95% CIs)
           mutate(mr = observed / expected,
                  mr.l95 = observed / (expected + qnorm(1-(.05/2)) * se),
                  mr.u95 = observed / (expected - qnorm(1-(.05/2)) * se)) %>%
           mutate(agegroup = y) %>%
           relocate(agegroup) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = age.counts.ls, y = names(age.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  write.xlsx(paste0(outdir, "excess_byage.xlsx"), asTable = TRUE)


#---- Subanalysis by Cause of Death #----

# Counting deaths by week per age group
cause.counts <- compute_counts(dat,
                               demo = demo.total,
                               date = "isodate",
                               by = "cause")


# Fitting model to calculate excess mortality
cause.counts.ls <- split(cause.counts, f = cause.counts$cause)
weekly.xs.cause <- lapply(cause.counts.ls, function(x) {
  return(
    x %>%
      excess_model(exclude = pandemic.dates,
                   start = min(.$date),
                   end = max(.$date),
                   knots.per.year = 12,
                   include.trend = FALSE,
                   frequency = 52,
                   model = "quasipoisson",
                   alpha = 0.05)
  )
})

# Restructuring for plotting (stacking obs vs exp)
weekly.cause.plotdat <- lapply(weekly.xs.cause, function(x) {
  # Restructuring model output (list) to dataframe for ease of use (dropping stuff
  # we're not interested in)
  x <- bind_cols(x[1:4]) %>%
    # Calculating 95% confidence intervals
    mutate(exp.l95 = exp(log(expected) - qnorm(1-(.05/2)) * log_expected_se),
           exp.u95 = exp(log(expected) + qnorm(1-(.05/2)) * log_expected_se))
  
  return(x %>%
           # Setting starting time of plot
           filter(date > make_date(2020, 01, 01) &
                    date < make_date(2020, 12, 27)) %>%
           # Stacking observed and expected to make ggplot creation easier
           pivot_longer(cols = observed:expected,
                        names_to = "class",
                        values_to = "value") %>%
           mutate(class = factor(class, levels = c("observed", "expected"),
                                 labels = c("Observed", "Expected"))))
})

# Plotting observed vs expected
xs.cause.plots <- mapply(function(x, y) {
  p <- ggplot(x, aes(x = date, y = value, col = class)) +
    geom_line() +
    geom_ribbon(data = x %>% filter(class == "Expected"),
                aes(ymin = exp.l95, ymax = exp.u95),
                fill = NA, lty = 2) +
    ylim(min(c(x$exp.l95, x$value)) - 5, max(x$value) + 5) +
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
  ggsave(paste0(outdir, y, "excess.png"), p, height = 2.5, width = 6.5, dpi = 300, units = "in")
  
  return(p)
}, x = weekly.cause.plotdat, y = names(weekly.cause.plotdat), SIMPLIFY = FALSE)

do.call(ggarrange, c(xs.cause.plots, ncol = 1))
ggsave(paste0(outdir, "excess_bycause.png"), height = 40, width = 6.5, dpi = 300, units = "in")

# Calculating overall excess during pandemic by cause of death
mapply(function(x, y) {
  return(x %>%
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
                  excess.u95 = excess + qnorm(1-(.05/2)) * se) %>%mutate(cause = y) %>%
           # Calculate mortality ratios (& 95% CIs)
           mutate(mr = observed / expected,
                  mr.l95 = observed / (expected + qnorm(1-(.05/2)) * se),
                  mr.u95 = observed / (expected - qnorm(1-(.05/2)) * se)) %>%
           relocate(cause) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = cause.counts.ls, y = names(cause.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  write.xlsx(paste0(outdir, "excess_bycause.xlsx"), asTable = TRUE)


#---- Subanalysis by County #----

# Approximate annual age demographics
demo.county <- approx_demographics(demo.co.in,
                                   first_day = min(dat$isodate),
                                   last_day = max(dat$isodate),
                                   extrapolation.type = "linear")

# Counting deaths by week per age group
county.counts <- compute_counts(dat,
                                demo = demo.county,
                                date = "isodate",
                                by = "county")


# Fitting model to calculate excess mortality
county.counts.ls <- split(county.counts, f = county.counts$county)
weekly.xs.county <- lapply(county.counts.ls, function(x) {
  return(
    x %>%
      excess_model(exclude = pandemic.dates,
                   start = min(.$date),
                   end = max(.$date),
                   knots.per.year = 12,
                   include.trend = FALSE,
                   frequency = 52,
                   model = "quasipoisson",
                   alpha = 0.05)
  )
})

# Calculating overall excess during pandemic by county
total.xs.county <- mapply(function(x, y) {
  return(x %>%
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
           # Calculate mortality ratios (& 95% CIs)
           mutate(mr = observed / expected,
                  mr.l95 = observed / (expected + qnorm(1-(.05/2)) * se),
                  mr.u95 = observed / (expected - qnorm(1-(.05/2)) * se)) %>%
           mutate(county = y) %>%
           relocate(county) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = county.counts.ls, y = names(county.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows()

total.xs.county %>%
  write.xlsx(paste0(outdir, "excess_bycounty.xlsx"), asTable = TRUE)
  
# Downloading county shapefile
co.sf <- tigris::counties(state = "MI", cb = TRUE) %>%
  mutate(county = paste(NAMELSAD, STUSPS, sep = ", ")) %>%
  left_join(total.xs.county, by = "county")

xs.map <- tm_shape(co.sf) +
  tm_polygons(fill = "xs_rate",
              fill.scale = tm_scale_continuous(values = "brewer.reds"))
xs.map %>%
  tmap_save(paste0(outdir, "excess_county_map.png"),
            width = 8, height = 8, dpi = 300, units = "in")
