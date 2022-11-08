##### 2: Prepare data #####
##### Characterizing responsiveness to the COVID-19 pandemic in the United States and Canada using mobility data #####
##### Soucy et al. #####
##### Script by Jean-Paul R. Soucy #####
##### https://github.com/jeanpaulrsoucy/covid-19-mobility-responsiveness #####

# load packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)

# create directory for processed data
dir.create("data", showWarnings = FALSE)

# load data

## case data
cdc <- read_csv("raw/cdc.csv")
phac <- read_csv("raw/phac.csv")

## mobility data
mob <- c(
  "raw/2020_CA_Region_Mobility_Report.csv", "raw/2021_CA_Region_Mobility_Report.csv",
  "raw/2020_US_Region_Mobility_Report.csv", "raw/2021_US_Region_Mobility_Report.csv") %>%
  map_dfr(read_csv)

# define constants

## Canadian P/T names and abbreviations
provs <- c(
  "Alberta" = "AB",
  "British Columbia" = "BC",
  "Manitoba" = "MB",
  "New Brunswick" = "NB",
  "Newfoundland and Labrador" = "NL",
  "Northwest Territories" = "NT",
  "Nova Scotia" = "NS",
  "Nunavut" = "NU",
  "Ontario" = "ON",
  "Prince Edward Island" = "PE",
  "Quebec" = "QC",
  "Saskatchewan" = "SK",
  "Yukon" = "YT"
)

## US state names and abbreviations
states <- setNames(
  state.abb,
  state.name
)

# load and process population data
pop_can <- read_csv("pop/pop-can.csv") %>%
  transmute(
    region = recode(GEO, !!!provs),
    pop = VALUE
  )
pop_usa <- read_csv("pop/pop-usa.csv") %>%
  filter(GEO %in% names(states)) %>%
  transmute(
    region = recode(GEO, !!!states),
    pop = VALUE
  )
pop <- bind_rows(pop_can, pop_usa)

# load US Census region data
usc_regions <- read_csv("pop/us-census-regions-divisions.csv") %>%
  select(
    region = `State Code`,
    region_group = Region
  )

# process case data

## CDC
cdc <- cdc %>%
  transmute(
    country = "United States",
    region = case_when(
      # combine NYC and rest of NY
      state %in% c("NY", "NYC") ~ "NY",
      TRUE ~ state
    ),
    date = as.Date(submission_date, "%m/%d/%Y"),
    cumulative_cases = as.integer(tot_cases),
    cumulative_deaths = as.integer(tot_death)
  ) %>%
  # combine NY and NYC
  group_by(country, region, date) %>%
  summarize(
    cumulative_cases = sum(cumulative_cases),
    cumulative_deaths = sum(cumulative_deaths),
    .groups = "drop_last") %>%
  arrange(country, region, date) %>%
  mutate(
    cases = c(first(cumulative_cases), diff(cumulative_cases)),
    deaths = c(first(cumulative_deaths), diff(cumulative_deaths))) %>%
  ungroup() %>%
  # limit to states
  filter(region %in% states)

## PHAC
phac <- phac %>%
  filter(prname %in% names(provs)) %>%
  complete(prname, date) %>%
  transmute(
    region = recode(prname, !!!provs),
    date = as.Date(date),
    cumulative_cases = as.integer(numtotal),
    cumulative_deaths = as.integer(numdeaths)
  )
date_seq <- seq.Date(from = min(phac$date), to = max(phac$date), by = "day")
phac <- phac %>%
  right_join(
    data.frame(
      region = rep(unique(phac$region), each = length(date_seq)),
      date = rep(date_seq, times = length(unique(phac$region)))
    ),
    by = c("region", "date")
  ) %>%
  arrange(region, date) %>%
  group_by(region) %>%
  fill(cumulative_cases, cumulative_deaths, .direction = "down") %>%
  replace_na(list(
    cumulative_cases = as.integer(0),
    cumulative_deaths = as.integer(0))) %>%
  mutate(
    country = "Canada",
    cases = c(first(cumulative_cases), diff(cumulative_cases)),
    deaths = c(first(cumulative_deaths), diff(cumulative_deaths))) %>%
  ungroup()

## combine case data and summarize daily data into weekly data
cases <- bind_rows(cdc, phac) %>%
  mutate(epi_week_year = paste(epiweek(date), epiyear(date), sep = "-")) %>%
  # start data in epi week 9 of 2020 (February 23, 2020)
  filter(date >= as.Date("2020-02-23")) %>%
  group_by(country, region, epi_week_year) %>%
  summarize(
    date = min(date),
    cases = sum(cases),
    cumulative_cases = max(cumulative_cases),
    deaths = sum(deaths),
    cumulative_deaths = max(cumulative_deaths),
    .groups = "drop") %>%
  arrange(country, region, date)

## calculate case rates (per 100k) and lagged cases/case rates
cases <- left_join(
  cases,
  pop,
  by = "region"
) %>%
  group_by(country, region) %>%
  mutate(
    cases_rate = cases / pop * 1000,
    cumulative_cases_rate = cumulative_cases / pop * 1000,
    cases_lag_1 = lag(cases, n = 1),
    cases_lag_2 = lag(cases, n = 2),
    cases_lag_3 = lag(cases, n = 3),
    cases_lag_4 = lag(cases, n = 4),
    cumulative_cases_lag = lag(cumulative_cases),
    cases_rate_lag_1 = lag(cases_rate, n = 1),
    cases_rate_lag_2 = lag(cases_rate, n = 2),
    cases_rate_lag_3 = lag(cases_rate, n = 3),
    cases_rate_lag_4 = lag(cases_rate, n = 4),
    cumulative_cases_rate_lag = lag(cumulative_cases_rate),
    deaths_rate = deaths / pop * 100000,
    cumulative_deaths_rate = cumulative_deaths / pop * 100000,
    cumulative_deaths_rate_lead_4 = lead(cumulative_deaths_rate, n = 4)
  ) %>%
  ungroup()

## add region group data
cases <- cases %>%
  left_join(
    usc_regions,
    by = "region"
  ) %>%
  replace_na(list(region_group = "Canada"))

## calculate change in cases (absolute and % change)
cases <- cases %>%
  group_by(country, region) %>%
  mutate(
    cases_lag_abs = c(NA, diff(cases_lag_1)),
    cases_lag_pct = cases_lag_abs / cases_lag_2 * 100,
    # convert NaN and Inf to NA
    cases_lag_pct = ifelse(is.nan(cases_lag_pct) | is.infinite(cases_lag_pct),
                           NA,
                           cases_lag_pct),
    cases_rate_lag_abs = c(NA, diff(cases_rate_lag_1)),
    cases_rate_lag_pct = cases_rate_lag_abs / cases_rate_lag_2 * 100,
    # convert NaN and Inf to NA
    cases_rate_lag_pct = ifelse(is.nan(cases_rate_lag_pct) | is.infinite(cases_rate_lag_pct),
                                NA,
                                cases_rate_lag_pct)
  ) %>%
  ungroup()

# process mobility data
mob <- mob %>%
  filter(!is.na(sub_region_1) & is.na(sub_region_2)) %>%
  mutate(
    country = case_when(
      country_region_code == "CA" ~ "Canada",
      country_region_code == "US" ~ "United States"
    ),
    region = case_when(
      country == "Canada" ~ recode(sub_region_1, !!!provs),
      country == "United States" ~ recode(sub_region_1, !!!states)
    ),
    date = as.Date(date),
    epi_week_year = paste(epiweek(date), epiyear(date), sep = "-")
  ) %>%
  select(c(country, region, epi_week_year, date,
           retail_and_recreation_percent_change_from_baseline)) %>%
  # start data in epi week 9 of 2020 (February 23, 2020)
  filter(date >= as.Date("2020-02-23")) %>%
  group_by(country, region, epi_week_year) %>%
  summarize(
    date = min(date),
    retail_and_recreation_percent_change_from_baseline = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(country, region, date) %>%
  group_by(country, region) %>%
  mutate(
    week = rep(seq.int(1, length(unique(date))), times = length(unique(region))),
    retail_and_recreation_percent_change_from_baseline_abs = c(NA, diff(retail_and_recreation_percent_change_from_baseline)),
  ) %>%
  ungroup()

# join case and mobility data
dat <- inner_join(
  cases,
  mob,
  by = c("country", "region", "epi_week_year", "date")
)

# add other variables
dat <- dat %>%
  mutate(
    # did lagged weekly incidence rise or fall/stay constant compared the the week prior?
    cases_rising = factor(
      case_when(
        is.na(cases_lag_1) ~ NA_real_,
        cases > cases_lag_1 ~ 1,
        TRUE ~ 0
      )),
    cases_rising_1 = factor(
      case_when(
        is.na(cases_lag_2) ~ NA_real_,
        cases_lag_1 > cases_lag_2 ~ 1,
        TRUE ~ 0
      )),
    cases_rising_2 = factor(
      case_when(
        is.na(cases_lag_3) ~ NA_real_,
        cases_lag_2 > cases_lag_3 ~ 1,
        TRUE ~ 0
      )),
    cases_rising_3 = factor(
      case_when(
        is.na(cases_lag_4) ~ NA_real_,
        cases_lag_3 > cases_lag_4 ~ 1,
        TRUE ~ 0
      )),
    # year-month
    year_month = substr(date, 1, 7),
    # month as integer
    month = month(date),
    # months since January 2020 (0)
    months_since_jan_2020 = factor(as.integer((year(date) - 2020) * 12 + month(date) - 1))
  )

# filter out bad data
dat <- dat %>%
  # regions with bad mobility data
  filter(!region %in% c("PE", "NT", "NU", "YT")) %>%
  # weeks with negative (lagged) cases
  # (cases_lag_2 as well since it would mess up the difference calculations)
  filter(cases_lag_1 >= 0 & !is.na(cases_lag_1) & cases_lag_2 >= 0)

# adjudicate massive percent changes in cases during period of interest (2020-12-01 to 2021-11-30)

## plotting function
plot_large_change <- function(dat, var_name, x) {
  # filter to state/province of x
  dat <- dat %>% filter(region == x$region)
  p <- ggplot() +
    geom_line(data = dat, aes(x = date, y = !!sym(var_name))) +
    geom_point(data = dat, aes(x = date, y = !!sym(var_name))) +
    geom_vline(xintercept = x$date) +
    labs(title = paste(x$region, x$date, sep = " / ")) +
    theme(plot.title = element_text(hjust = 0.5))
  plot(p)
}

## enumerate large (>300% in a week) changes during period of interest
large_changes_cases <- dat %>%
  filter(date >= as.Date("2020-12-01") & date <= as.Date("2021-11-30")) %>%
  filter(!is.na(cases_lag_pct) & abs(cases_lag_pct) > 300)

## plot each large change in context
for (i in 1:nrow(large_changes_cases)) {
  plot_large_change(dat, "cases_lag_1", large_changes_cases[i, ])
}

## remove observations with clear data errors
dat <- dat %>%
  filter(
    !(
      (region == "CA" & date == as.Date("2021-07-11"))
    )
  )

# adjudicate massive temporary changes in retail & recreation mobility during period of interest (2020-12-01 to 2021-11-30)

## enumerate large (+/- 10 points) changes during period of interest in which next week is also followed by a large change with the opposite sign
large_changes_rrmob <- dat %>%
  filter(date >= as.Date("2020-12-01") & date <= as.Date("2021-11-30")) %>%
  filter(
    !is.na(retail_and_recreation_percent_change_from_baseline_abs) &
      abs(retail_and_recreation_percent_change_from_baseline_abs) > 10 &
      abs(lead(retail_and_recreation_percent_change_from_baseline_abs)) > 10 &
      sign(retail_and_recreation_percent_change_from_baseline_abs) != sign(lead(retail_and_recreation_percent_change_from_baseline_abs)))

## plot each large change in context
for (i in 1:nrow(large_changes_rrmob)) {
  plot_large_change(dat, "retail_and_recreation_percent_change_from_baseline", large_changes_rrmob[i, ])
}

## remove observations with clear data errors (and week after, since they suffer the same error with opposite sign)
dat <- dat %>%
  filter(
    !(
      (region == "AR" & date == as.Date("2021-02-14")) |
        (region == "AR" & date == as.Date("2021-02-14") + 7) |
        (region == "LA" & date == as.Date("2021-02-14")) |
        (region == "LA" & date == as.Date("2021-02-14") + 7) |
        (region == "LA" & date == as.Date("2021-08-29")) |
        (region == "LA" & date == as.Date("2021-08-29") + 7) |
        (region == "MS" & date == as.Date("2021-02-14")) |
        (region == "MS" & date == as.Date("2021-02-14") + 7) |
        (region == "OK" & date == as.Date("2021-02-07")) | # identified from plot
        (region == "OK" & date == as.Date("2021-02-14")) |
        (region == "OK" & date == as.Date("2021-02-14") + 7) |
        (region == "TN" & date == as.Date("2021-02-14")) |
        (region == "TN" & date == as.Date("2021-02-14") + 7) |
        (region == "TX" & date == as.Date("2021-02-14")) |
        (region == "TX" & date == as.Date("2021-02-14") + 7)
    )
  )

# rearrange columns for export
dat <- dat %>%
  select(
    country,
    region,
    region_group,
    date,
    year_month,
    month,
    months_since_jan_2020,
    week,
    cases,
    cases_lag_1,
    cases_lag_2,
    cases_lag_3,
    cases_lag_4,
    cases_lag_abs,
    cases_lag_pct,
    cases_rising,
    cases_rising_1,
    cases_rising_2,
    cases_rising_3,
    cases_rate,
    cases_rate_lag_1,
    cases_rate_lag_2,
    cases_rate_lag_3,
    cases_rate_lag_4,
    cases_rate_lag_abs,
    cases_rate_lag_pct,
    retail_and_recreation_percent_change_from_baseline,
    retail_and_recreation_percent_change_from_baseline_abs,
    epi_week_year,
    pop,
    cumulative_cases,
    cumulative_cases_lag,
    cumulative_cases_rate,
    cumulative_cases_rate_lag,
    deaths,
    deaths_rate,
    cumulative_deaths,
    cumulative_deaths_rate,
    cumulative_deaths_rate_lead_4
)

# remove first week of data since change in mobility cannot be calculated (2020-02-23)
dat <- dat %>%
  filter(date != as.Date("2020-02-23"))

# for cases, turn negative values into NA
dat <- dat %>%
  mutate(across(c(
    cases, cases_lag_1, cases_lag_2, cases_lag_3, cases_lag_4,
    cases_rate, cases_rate_lag_1, cases_rate_lag_2, cases_rate_lag_3, cases_rate_lag_4),
    ~ ifelse(.x < 0, NA, .x)))

# for logged variables, replace -Inf and NaN with NA
dat <- dat %>%
  mutate(across(ends_with("_log"), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))

# export final dataset
write.csv(dat, "data/cases_mobility.csv", row.names = FALSE)
