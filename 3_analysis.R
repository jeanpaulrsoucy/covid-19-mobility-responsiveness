##### 3: Create figures and tables #####
##### Characterizing responsiveness to the COVID-19 pandemic in the United States and Canada using mobility data #####
##### Soucy et al. #####
##### Script by Jean-Paul R. Soucy #####
##### https://github.com/jeanpaulrsoucy/covid-19-mobility-responsiveness #####

# create directory for outputs
dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# load functions
source("funs.R")

# load data
dat_raw <- load_data_cases_mobility()

# filter data to period of interest
dat <- dat_raw %>%
  filter(
    date >= as.Date("2020-12-01") & date <= as.Date("2021-11-30"))

# get list of unique regions (w/ region group and country values)
regions <- dat %>%
  select(country, region_group, region) %>%
  distinct()

# descriptive plots

## beeswarm plot of case rate by month and country
ggplot(data = dat, aes(x = year_month, y = cases_rate, fill = country)) +
  geom_quasirandom(shape = 21, size = 1, alpha = 0.75) +
  facet_wrap(~country) +
  scale_y_continuous(expand = expansion(add = c(0, 0.5))) +
  scale_fill_grey() +
  labs(x = "Month", y = "Weekly case rate (per 1,000)") +
  theme_pubclean() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("figures/descriptive_case_rate_country.png", width = 6, height = 6)

## beeswarm plot of case rate by month in Canada (Atlantic versus rest of Canada)
dat_can <- dat %>%
  filter(country == "Canada") %>%
  mutate(atlantic = ifelse(region %in% c("NB", "NL", "NS"), "Atlantic Canada", "Rest of Canada"))
ggplot(data = dat_can, aes(x = year_month, y = cases_rate, fill = atlantic)) +
  geom_quasirandom(shape = 21, size = 1, alpha = 0.75) +
  facet_wrap(~atlantic) +
  scale_y_continuous(expand = expansion(add = c(0, 0.5))) +
  scale_fill_grey() +
  labs(x = "Month", y = "Weekly case rate (per 1,000)") +
  theme_pubclean() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("figures/descriptive_case_rate_atlantic.png", width = 6, height = 6)

# fit separate models for each region (retail and recreation)

## reference level: cases_rising = 1
mods_regs_rr <- fit_models_regions(regions, dat, rising_ref = "1")

## reference level: cases_rising = 0
mods_regs_rr_0 <- fit_models_regions(regions, dat, rising_ref = "0")

# extract slopes

## model: full_1
slopes_regs_rr_full_1 <- summarize_slopes_regions(regions, mods_regs_rr, "full_1")
slopes_regs_rr_0_full_1 <- summarize_slopes_regions(regions, mods_regs_rr_0, "full_1")

## model: intonly_1
slopes_regs_rr_intonly_1 <- summarize_slopes_regions(regions, mods_regs_rr, "intonly_1")
slopes_regs_rr_0_intonly_1 <- summarize_slopes_regions(regions, mods_regs_rr_0, "intonly_1")

# forest plots (country)
plot_meta(slopes_regs_rr_full_1, "country", file = "figures/meta_country_rr_rising_full_1.png", width = 8, height = 15)
plot_meta(slopes_regs_rr_0_full_1, "country", file = "figures/meta_country_rr_falling_full_1.png", width = 8, height = 15)
plot_meta(slopes_regs_rr_intonly_1, "country", file = "figures/meta_country_rr_rising_intonly_1.png", width = 8, height = 15)
plot_meta(slopes_regs_rr_0_intonly_1, "country", file = "figures/meta_country_rr_falling_intonly_1.png", width = 8, height = 15)

# forest plots (region group)
plot_meta(slopes_regs_rr_full_1, "region_group", file = "figures/meta_rg_rr_rising_full_1.png", width = 8, height = 18)
plot_meta(slopes_regs_rr_0_full_1, "region_group", file = "figures/meta_rg_rr_falling_full_1.png", width = 8, height = 18)
plot_meta(slopes_regs_rr_intonly_1, "region_group", file = "figures/meta_rg_rr_rising_intonly_1.png", width = 8, height = 18)
plot_meta(slopes_regs_rr_0_intonly_1, "region_group", file = "figures/meta_rg_rr_falling_intonly_1.png", width = 8, height = 18)

# meta-regression summary tables
table_meta_summary(regions, dat, "full") %>%
  write.csv("tables/meta_summary_full.csv", row.names = FALSE, na = "")
table_meta_summary(regions, dat, "intonly") %>%
  write.csv("tables/meta_summary_intonly.csv", row.names = FALSE, na = "")

# outcomes correlations and plots
write.table(NULL, file = "tables/cor_outcomes.csv", row.names = FALSE, col.names = FALSE) # create table for correlations

## model: full_1 / death rate (study period)
cor_outcomes(dat, slopes_regs_rr_full_1, "death rate (study period)") %>%
  write_cor_outcomes("rr_full_1", "death rate (study period)")
deaths_period_regs_rr_full_1 <- plot_outcomes(dat, slopes_regs_rr_full_1, "death rate (study period)", "log negative")
suppressWarnings({plot(deaths_period_regs_rr_full_1); ggsave("figures/deaths_period_regs_rr_rising_full_1.png", width = 6, height = 6)})
cor_outcomes(dat, slopes_regs_rr_0_full_1, "death rate (study period)") %>%
  write_cor_outcomes("rr_0_full_1", "death rate (study period)")
deaths_period_regs_rr_0_full_1 <- plot_outcomes(dat, slopes_regs_rr_0_full_1, "death rate (study period)", "log negative")
# suppressWarnings(plot(deaths_period_regs_rr_0_full_1))

## model: full_1 / cumulative death rate
cor_outcomes(dat, slopes_regs_rr_full_1, "cumulative death rate") %>%
  write_cor_outcomes("rr_full_1", "cumulative death rate")
deaths_cum_regs_rr_full_1 <- plot_outcomes(dat, slopes_regs_rr_full_1, "cumulative death rate", "log negative")
suppressWarnings({plot(deaths_cum_regs_rr_full_1); ggsave("figures/deaths_cum_regs_rr_rising_full_1.png", width = 6, height = 6)})
cor_outcomes(dat, slopes_regs_rr_0_full_1, "cumulative death rate") %>%
  write_cor_outcomes("rr_0_full_1", "cumulative death rate")
deaths_cum_regs_rr_0_full_1 <- plot_outcomes(dat, slopes_regs_rr_0_full_1, "cumulative death rate", "log negative")
# suppressWarnings(plot(deaths_cum_regs_rr_0_full_1))

## model: intonly_1 / death rate (study period)
cor_outcomes(dat, slopes_regs_rr_intonly_1, "death rate (study period)") %>%
  write_cor_outcomes("rr_intonly_1", "death rate (study period)")
deaths_period_regs_rr_intonly_1 <- plot_outcomes(dat, slopes_regs_rr_intonly_1, "death rate (study period)", "log negative")
suppressWarnings({plot(deaths_period_regs_rr_intonly_1); ggsave("figures/deaths_period_regs_rr_rising_intonly_1.png", width = 6, height = 6)})
cor_outcomes(dat, slopes_regs_rr_0_intonly_1, "death rate (study period)") %>%
  write_cor_outcomes("rr_0_intonly_1", "death rate (study period)")
deaths_period_regs_rr_0_intonly_1 <- plot_outcomes(dat, slopes_regs_rr_0_intonly_1, "death rate (study period)", "log negative")
# suppressWarnings(plot(deaths_period_regs_rr_0_intonly_1))

## model: intonly_1 / cumulative death rate
cor_outcomes(dat, slopes_regs_rr_intonly_1, "cumulative death rate") %>%
  write_cor_outcomes("rr_intonly_1", "cumulative death rate")
deaths_cum_regs_rr_intonly_1 <- plot_outcomes(dat, slopes_regs_rr_intonly_1, "cumulative death rate", "log negative")
suppressWarnings({plot(deaths_cum_regs_rr_intonly_1); ggsave("figures/deaths_cum_regs_rr_rising_intonly_1.png", width = 6, height = 6)})
cor_outcomes(dat, slopes_regs_rr_0_intonly_1, "cumulative death rate") %>%
  write_cor_outcomes("rr_0_intonly_1", "cumulative death rate")
deaths_cum_regs_rr_0_intonly_1 <- plot_outcomes(dat, slopes_regs_rr_0_intonly_1, "cumulative death rate", "log negative")
# suppressWarnings(plot(deaths_cum_regs_rr_0_intonly_1))

# outcomes correlations: average retail and recreation mobility
avg_mobility <- dat %>%
  group_by(region) %>%
  summarize(
    est = mean(retail_and_recreation_percent_change_from_baseline),
    .groups = "drop"
  )
cor_outcomes(dat, avg_mobility, "death rate (study period)") %>%
  write_cor_outcomes("avg_mobility", "death rate (study period)")
deaths_period_regs_rr_avg <- plot_outcomes(dat, avg_mobility, "death rate (study period)", "identity",
                                             x_lab = "Average weekly mobility (% of baseline)", x_percent = TRUE)
plot(deaths_period_regs_rr_avg); ggsave("figures/deaths_period_regs_rr_avg.png", width = 6, height = 6)
cor_outcomes(dat, avg_mobility, "cumulative death rate") %>%
  write_cor_outcomes("avg_mobility", "cumulative death rate")
deaths_cum_regs_rr_avg <- plot_outcomes(dat, avg_mobility, "cumulative death rate", "identity",
                                      x_lab = "Average weekly mobility (% of baseline)", x_percent = TRUE)
plot(deaths_cum_regs_rr_avg); ggsave("figures/deaths_cum_regs_rr_avg.png", width = 6, height = 6)
