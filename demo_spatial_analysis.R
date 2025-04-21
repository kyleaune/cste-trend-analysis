# This demo will (1) assign census tracts to community statistical areas (CSA),
# (2) download census tract population counts from the most recent American Community
# Survey (ACS) 5-year estimates using the US Census Bureau's API and the 'tidycensus'
# package, (3) calculate excess mortality by CSA, (4) map the pattern of excess
# mortality, and (5) create contextual maps of race to interpret these patterns w
# with Baltimore's sociodemographic structure.

#---- Setup #----

# Listing all the packages that are used
# tidyverse contains lots of useful data management and manipulation packages and functions
# excessmort is the main package used for the excess calculation analysis
# ISOweek helps convert week names to dates
# tidycensus uses the census API to download demographic data (api key is required
# and can be requested here: "https://api.census.gov/data/key_signup.html")
# tigris helps download shapefiles of census geographies (counties, census tracts, etc.)
# sf contains functions for spatial data processing
# tmap makes nice pretty maps from sf data (like ggplot for spatial stuff)
# cols4all contains lots of nice color scales to make pretty maps (and other plots too)
# scales helps rescale units when plotting
pkgs <- c("tidyverse", "excessmort", "ISOweek", "tidycensus", "tigris", "sf", "tmap",
          "cols4all", "scales")

# Installing any of the packages that you don't already have
install.packages(setdiff(pkgs, rownames(installed.packages())))

# Loading all the packages
lapply(pkgs, library, character.only = TRUE, quietly = TRUE, verbose = FALSE)

# Setting working directory
setwd("~/your_working_directory")

# Reading in full dataset
dat <- readRDS("deidentified_training_data_2016-2023.rda") %>%
  # Assigning each death to the week it occurred in
  mutate(isoweek = paste0(isoyear(DEATHDATE), "-W", sprintf("%02d", isoweek(DEATHDATE)),"-1")) %>%
  # Listing the date of the start of the week each death occurred
  mutate(isodate = ISOweek2date(isoweek))

# Downloading demographics at the county level
# Uses the census API via 'tidycensus' (requires API key)
# You can also read in your own locally stored demographic dataset as long as it
# has columns named for each of the variables you plan to subset by in your
# full dataset.
# Install your census API key with 'census_api_key(yourkeyhere)'
demo.co.dl <- get_demographics(geography = "county", state = "Maryland", county = "510",
                               years = seq(from = min(year(dat$isodate), na.rm = TRUE),
                                           to = max(year(dat$isodate), na.rm = TRUE),
                                           by = 1),
                               vars = c("SEX", "AGEGROUP", "RACE", "HISP"))

# Checking to make sure all years are downloaded
data.frame(
  year = seq(from = min(year(dat$isodate), na.rm = TRUE),
             to = max(year(dat$isodate), na.rm = TRUE),
             by = 1)) %>%
  mutate(dl = if_else(year %in% demo.co.dl$year, "Yes", "No"))
## 2023 data are missing (likely not released yet)

# Fixing the missing 2023 data by setting 2023 demographic values as the same as 2022 values
demo.co.23 <- demo.co.dl %>%
  filter(year == 2022) %>%
  mutate(year = 2023)
# Then adding these values to the end of the downloaded demographic file
demo.co.dl <- bind_rows(demo.co.dl, demo.co.23)

# The analysis needs a demographic record for every day in the analysis, so we
# create one here using 'excessmort::approx_demographics'
# extrapoloation.type can be linear (assume a linear change from year 1 to year 2
# and impute the change along the way) or constant (keep all days of year 1 the
# same)
demo.co <- approx_demographics(demo.co.dl, first_day = min(dat$isodate, na.rm = TRUE),
                               last_day = max(dat$isodate, na.rm = TRUE),
                               extrapolation.type = "linear")


#---- Setting Up Spatial Data #----

# Reading in CSA shapefile from Baltimore Neighborhood Indicators Alliance (BNIA)
csa.sf <- st_read("https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Community_Statistical_Areas_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
  # Renaming columns for easier merging
  rename(csa = "CSA2020")

# Reading in a relationship table from BNIA
csa.rel <- st_read("https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Tract2020_to_CSA2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
  # Renaming columns to something a little more convenient to type
  rename(tract = Tract_2020,
         GEOID = GEOID_Tract_2020,
         csa = Community_Statistical_Area_2020)

# Assigning each census tract to its CSA in the source data
dat  <- dat %>%
  left_join(csa.rel, by = c("DECEDENTCENSUS_TRACT" = "tract"))

# Downloading population counts only (not other demographics) at the tract level
# using the tidycensus package itself (rather than the version that's used in the
# 'get_demographics' function above) using 5-year ACS estimates
# These estimates will overlap with each other, so big changes in population size
# year-to-year will not appear as clearly, but shorter averaging periods (e.g. 1-
# or 3-year) are not accurate at areas smaller than a city or county
demo.tr.dl <- bind_rows(
  lapply(seq(from = min(year(dat$isodate), na.rm = TRUE),
             to = max(year(dat$isodate), na.rm = TRUE),
             by = 1), function(x) {
               return(
                 get_acs(geography = "tract",
                         state = "MD",
                         county = "Baltimore city",
                         variables = "DP05_0001",
                         year = x,
                         survey = "acs5") %>%
                   mutate(year = x)
               )
             }
  )
) %>%
  # Renaming populaiton variable to make sense
  rename(population = estimate)

# Checking to make sure all years are downloaded
data.frame(
  year = seq(from = min(year(dat$isodate), na.rm = TRUE),
             to = max(year(dat$isodate), na.rm = TRUE),
             by = 1)) %>%
  mutate(dl = if_else(year %in% demo.tr.dl$year, "Yes", "No"))
## Everything's here!

# Assigning each census tract to its CSA in the census population data
demo.tr.dl <- demo.tr.dl %>%
  left_join(csa.rel, by = "GEOID")

# Calculating total population by year in each CSA
demo.csa.dl <- demo.tr.dl %>%
  group_by(csa, year) %>%
  summarize(population = sum(population))

# The analysis needs a demographic record for every day in the analysis, so we
# create one here using 'excessmort::approx_demographics' as we did earlier for
# the full dataset
demo.csa <- approx_demographics(demo.csa.dl,
                                first_day = min(dat$isodate, na.rm = TRUE),
                                last_day = max(dat$isodate, na.rm = TRUE),
                                extrapolation.type = "linear")


#---- Excess Mortality Calculation by CSA #----

# Creating list of dates where we want to calculate excess (i.e. during the pandemic)
# This is basically all of the dates that we don't want to train our expected counts
pandemic.dates <- seq(make_date(2020, 3, 1), max(dat$DEATHDATE, na.rm = TRUE), by = "day")

# Counting deaths by week per CSA
csa.counts <- compute_counts(dat,
                             demo = demo.csa,
                             date = "isodate",
                             by = "csa") %>%
  # Dropping jail CSA
  filter(csa != "Unassigned -- Jail")

# Splitting full dataset by CSA
csa.counts.ls <- split(csa.counts,
                       f = csa.counts$csa)

# Calculating overall excess during pandemic by CSA
ovr.xs.csa <- mapply(function(x, y) {
  return(x %>%
           excess_model(exclude = pandemic.dates,
                        intervals = list(pandemic.dates),
                        knots.per.year = 12,
                        include.trend = FALSE,
                        model = "quasipoisson",
                        alpha = 0.05) %>%
           mutate(csa = y) %>%
           relocate(csa) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = csa.counts.ls, y = names(csa.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows() 

# Calculating overall excess by period
period.xs.csa <- mapply(function(x, y) {
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
           mutate(csa = y) %>%
           relocate(csa) %>%
           mutate(period = c("Full", "Year 1", "Year 2", "Year 3")) %>%
           relocate(period) %>%
           mutate(xs_rate = obs_death_rate - exp_death_rate))
}, x = csa.counts.ls, y = names(csa.counts.ls), SIMPLIFY = FALSE) %>%
  bind_rows()


#---- Mapping Patterns of Excess Mortality #----

# The model output includes a rate, but this is a person-time rate, not just a rate
# per capita. Let's merge the model output with the average CSA population count
# in our time period of interest (2020-2023).
csapop20.23 <- demo.csa.dl %>%
  filter(year >= 2020 & year <= 2023) %>%
  group_by(csa) %>%
  summarize(population = mean(population))

ovr.xs.csa <- ovr.xs.csa %>%
  left_join(csapop20.23, by = "csa") %>%
  # Calculating the rates of observed, expected, and excess death per 1,000
  mutate(across(.cols = c(observed:excess),
                .fns = ~ .x / population * 1000,
                .names = "{col}p1k"))

# Adding geometry to overall excess mortality counts
ovr.xs.csa.sf <- csa.sf %>%
  left_join(ovr.xs.csa, by = "csa")

# Quick choropleth
plot(ovr.xs.csa.sf[, "excessp1k"])

# Presentation quality map
tm_shape(ovr.xs.csa.sf) +
  tm_polygons(fill = "excessp1k")

# Let's add a layer underneath so we can turn the harbor blue
# We'll download a basic outline of the city from the Census Bureau's TIGER/Line
# database (no API key needed for this) using the 'tigris' package
balt <- counties(state = "MD", cb = FALSE) %>%
  # This downloads all of the counties, but we only need Baltimore city, so we'll
  # filter the results
  filter(COUNTYFP == "510") %>%
  # Now we need to make sure the coordinates of this map layer match the one we
  # have for CSAs (it should match since they likely came from the same place, 
  # but it never hurts to be sure)
  st_transform(st_crs(csa.sf))

# Let's check what we just downloaded to make sure it's what we're looking for
plot(balt)
  # That was kind of what we're looking for, but we really only wanted one picture
  # R will plot every variable of an sf object, so if we only want to see the shape
  # we need to specify that we only want to plot its geometry
plot(balt$geometry)
  # Much better!

# Let's add that to our nice looking choropleth 
tm_shape(ovr.xs.csa.sf) +
  tm_polygons(fill = "excessp1k") +
  tm_shape(balt) +
  tm_borders(fill = "lightblue")

# Well that doesn't look good... tmap layers the objects you list in order, so
# we accidentally put the full outline layer on top of our results, let's fix it
tm_shape(balt) +
  tm_borders(fill = "lightblue") +
  tm_shape(ovr.xs.csa.sf) +
  tm_polygons(fill = "excessp1k")

# The color scheme for our excess results is diverging (which is nice since it shows
# clearly that one color is good, the other is bad, which is true in this case),
# but it's pretty similar to the color we picked for the harbor (and water is
# usually blue, so I don't want to change that). Let's pick a new diverging scheme.
cols4all::c4a_gui()

# The 'pi_yg' scale from Matlab is colorblind friendly (which usually means that
# it prints well in greyscale) and doesn't have any blues in it, so let's go
# with that one. But since green would normally be considered good (green = go/money/green grass),
# let's switch the order by adding a minus sign when we specify that scale. Make
# sure to close out the popup window where we were looking at colors. R won't run
# any more code while it's busy with that window.
tm_shape(balt) +
  tm_borders(fill = "lightblue") +
  tm_shape(ovr.xs.csa.sf) +
  tm_polygons(fill = "excessp1k",
              fill.scale = tm_scale_intervals(n = 6,
                                              midpoint = 0,
                                              values = "-matplotlib.pi_yg"))

# Now let's clean up the appearance

# tmap tries to help us with formatting, but sometimes it's not exactly what we
# want. Let's turn off the autofit feature for map components so we can specify
# the sizes and locations of map elements
tmap_options(component.autoscale = FALSE)

# Since most of the readers of this map are based in the US, let's change the units
tm_shape(balt, unit = "mi") +
  tm_borders(fill = "lightblue") +
  tm_shape(ovr.xs.csa.sf) +
  tm_polygons(fill = "excessp1k",
              fill.scale = tm_scale_intervals(n = 6,
                                              midpoint = 0,
                                              values = "-matplotlib.pi_yg",
                                              # Renaming the excluded 'unassigned -- jail'
                                              # CSA from 'missing' to 'excluded' (since 
                                              # these data weren't actually missing)
                                              label.na = "Excluded"),
              # Adding a legend title, '\n' will insert a line break in our title
              fill.legend = tm_legend(title = "Excess Mortality\nper 1,000 Residents")) +
  # Adding a legend for the blue harbor water element
  tm_add_legend(type = "borders",
                labels = "Baltimore Harbor",
                fill = "lightblue") +
  # Adding a compass
  tm_compass(position = c("center", "BOTTOM")) +
  # Adding a scalebar (specifying breaks at 0, 1, 2, and 3 miles)
  tm_scalebar(breaks = seq(0, 4), position = c("left", "BOTTOM")) +
  # Adding a title
  tm_title("Excess Mortality in Baltimore, MD by CSA (2020-2023)",
           position = tm_pos_out("center", "top"))

# If we want to interact with the map we just created, we can change the mode
tmap_mode("view")

# And replot our map
tm_shape(balt, unit = "mi") +
  tm_borders(fill = "lightblue") +
  tm_shape(ovr.xs.csa.sf) +
  tm_polygons(fill = "excessp1k",
              fill.scale = tm_scale_intervals(n = 6,
                                              midpoint = 0,
                                              values = "-matplotlib.pi_yg",
                                              # Renaming the excluded 'unassigned -- jail'
                                              # CSA from 'missing' to 'excluded' (since 
                                              # these data weren't actually missing)
                                              label.na = "Excluded"),
              # Adding a legend title, '\n' will insert a line break in our title
              fill.legend = tm_legend(title = "Excess Mortality\nper 1,000 Residents")) +
  # Adding a legend for the blue harbor water element
  tm_add_legend(type = "borders",
                labels = "Baltimore Harbor",
                fill = "lightblue") +
  # Adding a compass
  tm_compass(position = c("center", "BOTTOM")) +
  # Adding a scalebar (specifying breaks at 0, 1, 2, and 3 miles)
  tm_scalebar(breaks = seq(0, 4), position = c("left", "BOTTOM")) +
  # Adding a title
  tm_title("Excess Mortality in Baltimore, MD\nby CSA (2020-2023)",
           position = tm_pos_out("center", "top"))

# Let's switch back to the plot mode (which we can use to save plots for sharing)
tmap_mode("plot")

# Let's save the plot that we just made so we can share it or include it in reports
xs.map <- tm_shape(balt, unit = "mi") +
  tm_polygons(fill = "lightblue") +
  tm_shape(ovr.xs.csa.sf) +
  tm_polygons(fill = "excessp1k",
              fill.scale = tm_scale_intervals(n = 6,
                                              midpoint = 0,
                                              values = "-matplotlib.pi_yg",
                                              label.na = "Excluded"),
              fill.legend = tm_legend(title = "Excess Mortality\nper 1,000 Residents")) +
  tm_add_legend(type = "polygons",
                labels = "Baltimore Harbor",
                fill = "lightblue") +
  tm_compass(position = c("center", "BOTTOM")) +
  tm_scalebar(breaks = seq(0, 4), position = c("left", "BOTTOM")) +
  tm_title("Excess Mortality in Baltimore, MD\nby CSA (2020-2023)",
           position = tm_pos_out("center", "top"))

tmap_save(xs.map,
          # Specifying the filename (this will appear in your working directory by
          # default, but you can specify any filepath you want)
          filename = "excessmap20-23.png",
          # Specifying the dimensions and resolution of the figure
          width = 6, height = 6, units = "in", dpi = 300)


#---- Contextual Maps #----

# To help us interpret patterns of events that we think may vary across demographic
# groups (age, race, etc.), let's download some demographic data from the US Census
# Bureau, make a map, and look at it side-by-side with our excess mortality results

# Let's download the most recent 5-year ACS data for Black and white race and Hispanic
# ethnicity in Baltimore at the tract level. To figure out which variable codes
# we want to pull in, we can use  'tidycensus::load_variables()'
View(load_variables(year = 2023, dataset = "acs5/profile"))
  ## Total - DP05_0001
  ## White - DP05_0037
  ## Black - DP05_0038
  ## Hispanic - DP05_0076

# Now let's download those variables
race.tr <- get_acs(geography = "tract",
                   state = "MD",
                   county = "Baltimore city",
                   year = 2023,
                   survey = "acs5",
                   variables = c("DP05_0001",
                                 "DP05_0037",
                                 "DP05_0038",
                                 "DP05_0076"),
                   # We want one column for each variable
                   output = "wide") %>%
  # Let's rename the codes to meaningful column names
  rename(total = DP05_0001E,
         white = DP05_0037E,
         black = DP05_0038E,
         hisp = DP05_0076E)

# Now let's add those tracts up to their corresponding CSAs
race.csa <- race.tr %>%
  left_join(csa.rel, by = "GEOID") %>%
  group_by(csa) %>%
  summarize(across(.cols = c(total, white, black, hisp),
                   .fns = ~ sum(.x),
                   .names = "{col}")) %>%
  # Calculating percentages
  mutate(across(.cols = c(white, black, hisp),
                .fns = ~ .x / total,
                .names = "{col}.per"))

# Now let's add geometry to that data
race.sf <- csa.sf %>%
  left_join(race.csa, by = "csa")

# Quick choropleths to see if our download, summing, and merging worked correctly
plot(race.sf[, "total"])
plot(race.sf[, "white.per"])
plot(race.sf[, "black.per"])
plot(race.sf[, "hisp.per"])

# Let's restructure the data into a long format so we can make some panel maps
race.long <- race.sf %>%
  pivot_longer(cols = c(white.per, black.per, hisp.per),
               names_to = "race",
               values_to = "percent") %>%
  # Let's convert the race column to a factor so it's ordered, then group Black
  # and white together (since they're both races), followed by Hispanic ethnicity
  mutate(race = factor(race,
                       levels = c("black.per",
                                  "white.per",
                                  "hisp.per")))

# Now let's repeat the same basic tmap code from above, but changing the color
# scale to continuous for 0-100% and adding panels for race composition of each CSA
race.map <- tm_shape(balt, unit = "mi") +
  tm_polygons(fill = "lightblue") +
  tm_shape(race.long) +
  tm_polygons(fill = "percent",
              fill.scale = tm_scale_continuous(limits = c(0, 1),
                                               values = "brewer.greens",
                                               # Formatting decimal % as 0%-100%
                                               label.format = list(fun = function(x) paste0(scales::percent(x)))),
              fill.legend = tm_legend(title = "CSA Makeup")) +
  tm_add_legend(type = "polygons",
                labels = "Baltimore Harbor",
                fill = "lightblue") +
  tm_compass(position = c("center", "BOTTOM")) +
  tm_scalebar(breaks = seq(0, 4), position = c("left", "BOTTOM")) +
  # We can repeat the plot by subgroups of our data using 'tm_facets' and naming the
  # group column
  tm_facets(by = "race") +
  # Let's rename the panels to be more presentable
  tm_layout(panel.labels = c("Black", "White", "Hispanic"),
            # And let's make sure the legend stays to the right of all the panels
            legend.position = tm_pos_out("right", "center")) +
  tm_title("Racial Structure of Baltimore CSAs (2023)",
           position = tm_pos_out("center", "top"))

tmap_save(race.map,
          "race2023.png",
          width = 12, height = 6, units = "in", dpi = 300)

# Let's combine the excess mortality with the race/ethnicity panel plot and save
tmap_save(
  tmap_arrange(xs.map +
                 # Let's remove the frame around this plot
                 tm_layout(frame = FALSE),
               race.map,
               nrow = 2,
               heights = c(0.6, 0.4)),
  "excess_with_race.png",
  width = 12, height = 12, units = "in", dpi = 300)

