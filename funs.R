##### Common functions #####
##### Characterizing responsiveness to the COVID-19 pandemic in the United States and Canada using mobility data #####
##### Soucy et al. #####
##### Script by Jean-Paul R. Soucy #####
##### https://github.com/jeanpaulrsoucy/covid-19-mobility-responsiveness #####

# load libraries
library(ggplot2)
library(scales, exclude = "col_factor") # avoid package conflict with readr if running scripts sequentially
library(ggrepel)
library(ggpubr)
library(ggbeeswarm)
library(dplyr)
library(readr)
library(meta)
library(metafor)
library(magick)

# load case & mobility data
load_data_cases_mobility <- function() {
  read_csv(
    file = "data/cases_mobility.csv",
    col_types = cols(
      country = col_factor(),
      region = col_factor(),
      region_group = col_factor(),
      date = col_date(),
      year_month = col_factor(),
      month = col_factor(levels = as.character(c(12, 1:11))),
      months_since_jan_2020 = col_integer(),
      week = col_integer(),
      cases = col_integer(),
      cases_lag_1 = col_integer(),
      cases_lag_2 = col_integer(),
      cases_lag_3 = col_integer(),
      cases_lag_4 = col_integer(),
      cases_lag_abs = col_double(),
      cases_lag_pct = col_double(),
      cases_rising = col_factor(levels = c("1", "0"), include_na = FALSE),
      cases_rising_1 = col_factor(levels = c("1", "0"), include_na = FALSE),
      cases_rising_2 = col_factor(levels = c("1", "0"), include_na = FALSE),
      cases_rising_3 = col_factor(levels = c("1", "0"), include_na = FALSE),
      cases_rate = col_double(),
      cases_rate_lag_1 = col_double(),
      cases_rate_lag_2 = col_double(),
      cases_rate_lag_3 = col_double(),
      cases_rate_lag_4 = col_double(),
      cases_rate_lag_abs = col_double(),
      cases_rate_lag_pct = col_double(),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      retail_and_recreation_percent_change_from_baseline_abs = col_double(),
      epi_week_year = col_factor(),
      pop = col_integer(),
      cumulative_cases = col_integer(),
      cumulative_cases_lag = col_integer(),
      cumulative_cases_rate = col_double(),
      cumulative_cases_rate_lag = col_double(),
      deaths = col_integer(),
      deaths_rate = col_double(),
      cumulative_deaths = col_integer(),
      cumulative_deaths_rate = col_double(),
      cumulative_deaths_rate_lead_4 = col_double()
      )
    )
}

# fit models with specified mobility variable and model type
fit_models <- function(dat, rising_ref = c("1", "0"), random_intercept = FALSE) {

  # rising_ref: reference level for cases_rising variable
  # 1: default (reference level is cases are rising compared to previous week)
  # 0: relevel (reference level is cases are the same or falling compared to previous week)

  # match arguments
  match.arg(rising_ref, c("1", "0"))

  # relevel cases_rising variables
  if (rising_ref == 0) {
    dat <- dat %>%
      mutate(
        cases_rising_1 = relevel(cases_rising_1, "0"),
        cases_rising_2 = relevel(cases_rising_2, "0"),
        cases_rising_3 = relevel(cases_rising_3, "0"))
  }

  # create list to hold outputs
  mods <- list()

  # outcome variable name
  outcome <- "retail_and_recreation_percent_change_from_baseline_abs"

  # general formulas - full model
  formulas_full <- sapply(1:3, function(n) {
    paste0(outcome, " ~ ", paste0("cases_rate_lag_", n), " + ", paste0("cases_rate_lag_", n), "*", paste0("cases_rising_", n))
  })

  # generate formulas - interaction only
  formulas_intonly <- sapply(1:3, function(n) {
    paste0(outcome, " ~ ", paste0("cases_rate_lag_", n), " + ", paste0("cases_rate_lag_", n), ":", paste0("cases_rising_", n))
  })

  # combine formulas
  formulas <- c(formulas_full, formulas_intonly)

  # run models
  mod_names <- c(paste0("full_", 1:3), paste0("intonly_", 1:3))
  for (i in 1:6) {
    mods[[mod_names[i]]] <- lm(data = dat, formula = as.formula(formulas[i]))
  }

  # return models
  mods
}

# extract main slope coefficient (and SE) from the output of fit_models() for a specific model type
extract_slopes <- function(mod) {
  # extract coefficient from model
  out <- data.frame(
    est = coef(summary(mod))[2, 1:2][1],
    se = coef(summary(mod))[2, 1:2][2]
  )
  # return data frame
  out
}

# run fit_models() for a given set of regions
fit_models_regions <- function(regions, dat, rising_ref = c("1", "0")) {
  # fit models
  mods <- lapply(1:nrow(regions), function(x) {
    fit_models(dat[dat$region == as.character(regions[[x, "region"]]), ],
               rising_ref, random_intercept = FALSE)
  })
  # add names to list
  names(mods) <- as.character(regions[["region"]])
  # return list
  mods
}

# create table of slopes for a specific model type (e.g., noint_1, full_2, etc.) for all regions
summarize_slopes_regions <- function(regions, mods, mod) {
  # match arguments
  match.arg(mod, c("intonly_1", "intonly_2", "intonly_3", "full_1", "full_2", "full_3"))
  # extract slopes
  out <- lapply(1:nrow(regions), function(x) {
    extract_slopes(mods[[regions[[x, "region"]]]][[mod]])
  })
  # format output
  out <- bind_rows(out) %>%
    bind_cols(regions, .)
  # return output
  out
}

# fit meta-regression
fit_meta <- function(dat, subgroup = c("region_group", "country")) {
  # match arguments
  match.arg(subgroup, c("region_group", "country"), several.ok = FALSE)
  # improve scale of data for plotting (multiply coefficient and SE by rescaling factor)
  dat$est <- dat$est
  dat$se <- dat$se
  # fit meta-regression
  meta_mod <- metagen(TE = est, seTE = se, data = dat,
                      subgroup = dat[[subgroup]],
                      sm = "MD", method.tau = "REML")
  # return model
  meta_mod
}

# extract subgroup estimates and CI from meta-regression model
extract_subgroup_estimates <- function(meta_mod, extract_overall = FALSE) {
  # set precision
  precision <- "%.2f"
  # just extract overall estimate if extract_overall == TRUE,
  # else extract subgroup estimates
  if (extract_overall) {
    est <- sprintf(precision, meta_mod$TE.random)
    lower <- sprintf(precision, meta_mod$lower.random)
    upper <- sprintf(precision, meta_mod$upper.random)
  } else {
    est <- sprintf(precision, meta_mod$TE.random.w)
    lower <- sprintf(precision, meta_mod$lower.random.w)
    upper <- sprintf(precision, meta_mod$upper.random.w)
  }
  paste0(est, " (", lower, ", ", upper, ")")
}

# extract p value for test of subgroup differences from meta-regression model
extract_subgroup_test <- function(meta_mod) {
  p <- sprintf("%.4f", meta_mod$pval.Q.b.random)
  if (p == "0.0000") {
    "<0.0001"
  } else {
    p
  }
}

# fit meta-regression and return forest plot
plot_meta <- function(dat, subgroup = c("region_group", "country"), file, width, height) {
  # match arguments
  match.arg(subgroup, c("region_group", "country"), several.ok = FALSE)
  # set x limits to plot
  xlim <- c(-5, 2) # more reasonable x-axis limits
  # set digits
  digits <- 2
  # fit meta-regression
  meta_mod <- fit_meta(dat, subgroup)
  # pick subgroup label
  if (subgroup == "region_group") {
    lab_subgroup <- "Region group"
  } else {
    lab_subgroup <- "Country"
  }
  # open graphics device
  png(file = file, width = width, height = height, units = "in", res = 300)
  # plot forest plot
  forest(
    meta_mod,
    xlim = xlim,
    study.results = TRUE,
    subgroup.name = "",
    studlab = as.character(dat$region),
    sortvar = TE,
    fixed = FALSE,
    rightcols = FALSE,
    leftcols = c("studlab", "effect", "ci"),
    leftlabs = c(
      lab_subgroup, "Beta coefficient", "95% C.I."),
    xlab = "Mean difference",
    smlab = "",
    weight.study = "random",
    squaresize = 1.5,
    col.square = "navy",
    col.square.lines = "navy",
    col.diamond = "maroon",
    col.diamond.lines = "maroon",
    pooled.totals = TRUE,
    comb.fixed = FALSE,
    fs.hetstat = 10,
    print.tau2 = TRUE,
    print.Q = TRUE,
    print.pval.Q = TRUE,
    print.I2 = TRUE,
    digits = digits,
    col.by = "black",
    show_weights = FALSE)
  # close graphics device
  dev.off()
  # rescale image to 4 inch width
  i <- image_read(file)
  i <- image_scale(i, "1200")
  image_set_defines(i, c("png:exclude-chunk" = "date,time")) # prevent binary from changing with each run
  image_write(i, file)
}

# build summary table for meta-regressions for a specific model type
table_meta_summary <- function(regions, dat, mods) {
  # match arguments
  match.arg(mods, c("full", "intonly"))
  if (mods == "full") {
    mods <- c("full_1", "full_2", "full_3")
  } else {
    mods <- c("intonly_1", "intonly_2", "intonly_3")
  }
  # fit models (reference level: cases_rising = 1)
  mods_1 <- fit_models_regions(regions, dat, rising_ref = "1")
  # fit models (reference level: cases_rising = 0)
  mods_0 <- fit_models_regions(regions, dat, rising_ref = "0")
  # extract slopes
  slopes_1 <- lapply(seq_along(mods), function(x) {
    summarize_slopes_regions(regions, mods_1, mods[x])
  })
  slopes_0 <- lapply(seq_along(mods), function(x) {
    summarize_slopes_regions(regions, mods_0, mods[x])
  })
  # fit meta-regression models
  meta_mods_1 <- lapply(seq_along(slopes_1), function(x) {
    list(
      region_group = fit_meta(slopes_1[[x]], "region_group"),
      country = fit_meta(slopes_1[[x]], "country")
    )
  })
  meta_mods_0 <- lapply(seq_along(slopes_0), function(x) {
    list(
      region_group = fit_meta(slopes_0[[x]], "region_group"),
      country = fit_meta(slopes_0[[x]], "country")
    )
  })
  # assemble summary table: overall
  table_overall <- data.frame(
    subgroup = "Overall",
    rising_1 = "",
    falling_1 = "",
    rising_2 = "",
    falling_2 = "",
    rising_3 = "",
    falling_3 = ""
  )
  for (i in seq_along(mods)) {
    table_overall[, i * 2] <- c(extract_subgroup_estimates(meta_mods_1[[i]]$region_group, extract_overall = TRUE))
    table_overall[, i * 2 + 1] <- c(extract_subgroup_estimates(meta_mods_0[[i]]$region_group, extract_overall = TRUE))
  }
  # assemble summary table: countries
  table_country <- data.frame(
    subgroup = c(levels(regions$country), "Test for subgroup differences"),
    rising_1 = "",
    falling_1 = "",
    rising_2 = "",
    falling_2 = "",
    rising_3 = "",
    falling_3 = ""
  )
  for (i in seq_along(mods)) {
    table_country[, i * 2] <- c(extract_subgroup_estimates(meta_mods_1[[i]]$country), extract_subgroup_test(meta_mods_1[[i]]$country))
    table_country[, i * 2 + 1] <- c(extract_subgroup_estimates(meta_mods_0[[i]]$country), extract_subgroup_test(meta_mods_0[[i]]$country))
  }
  # assemble summary table: region groups
  table_rg <- data.frame(
    subgroup = c(levels(regions$region_group), "Test for subgroup differences"),
    rising_1 = "",
    falling_1 = "",
    rising_2 = "",
    falling_2 = "",
    rising_3 = "",
    falling_3 = ""
  )
  for (i in seq_along(mods)) {
    table_rg[, i * 2] <- c(extract_subgroup_estimates(meta_mods_1[[i]]$region_group), extract_subgroup_test(meta_mods_1[[i]]$region_group))
    table_rg[, i * 2 + 1] <- c(extract_subgroup_estimates(meta_mods_0[[i]]$region_group), extract_subgroup_test(meta_mods_0[[i]]$region_group))
  }
  # merge tables and return
  bind_rows(table_overall,
            data.frame(subgroup = "Countries"),
            table_country,
            data.frame(subgroup = "Region groups"),
            table_rg)
}

# calculate Spearman correlation for health outcomes and slopes
cor_outcomes <- function(dat, slopes, outcome) {
  # match arguments
  match.arg(outcome, c("cumulative death rate", "death rate (study period)"))
  if (outcome == "cumulative death rate") {
    dat <- dat %>%
      group_by(region) %>%
      slice_tail() %>%
      ungroup()
    outcome_var <- "cumulative_deaths_rate_lead_4"
  } else {
    dat <- dat %>%
      group_by(region) %>%
      mutate(cumulative_deaths_rate_period =
               max(cumulative_deaths_rate_lead_4) - min(cumulative_deaths_rate_lead_4)) %>%
      slice_tail() %>%
      ungroup()
    outcome_var <- "cumulative_deaths_rate_period"
  }
  dat <- dat %>%
    left_join(
      slopes %>%
        select(region, est),
      by = "region"
    ) %>%
  transmute(est, out = !!sym(outcome_var))
  # format results
  correlation <- sprintf("%.2f", cor(dat$est, dat$out, method = "spearman"))
  # print result
  print(correlation)
  # return result
  correlation
}

# write correlation to table
write_cor_outcomes <- function(correlation, mod, outcome) {
  write.table(t(c(mod, outcome, correlation)), file = "tables/cor_outcomes.csv",
              col.names = FALSE, row.names = FALSE, sep = ",", append = TRUE)
}


# plot health outcomes versus slopes
plot_outcomes <- function(dat, slopes, outcome, slope_type, x_lab = NULL, x_percent = FALSE) {
  # match arguments
  match.arg(outcome, c("cumulative death rate", "death rate (study period)"))
  match.arg(slope_type, c("identity", "log negative"))
  # get most recent death data
  if (outcome == "cumulative death rate") {
    dat <- dat %>%
      group_by(region) %>%
      slice_tail() %>%
      ungroup()
    outcome_var <- "cumulative_deaths_rate_lead_4"
    outcome_lab <- "Cumulative death rate per 100,000"
  } else {
    dat <- dat %>%
      group_by(region) %>%
      mutate(cumulative_deaths_rate_period =
               max(cumulative_deaths_rate_lead_4) - min(cumulative_deaths_rate_lead_4)) %>%
      slice_tail() %>%
      ungroup()
    outcome_var <- "cumulative_deaths_rate_period"
    outcome_lab <- "Death rate per 100,000"
  }
  # transform slopes
  if (slope_type == "log negative") {
    suppressWarnings({
      slopes$est <- -slopes$est
      # report regions excluded from the plot
      nan_regions <- slopes %>%
        mutate(est = log(est)) %>%
        filter(is.nan(est)) %>%
        pull(region)
    })
    if (length(nan_regions) > 0) {
      warning("Removing regions with NaN slopes: ", paste(nan_regions, collapse = ", "))
    }
  }
  # join slopes to data
  dat <- dat %>% left_join(
    slopes %>%
      select(region, est),
    by = "region"
  )
  # set x label
  if (is.null(x_lab)) {
    x_lab <- "Beta coefficient"
  }
  # plot outcomes
  p <- ggplot(data = dat, aes(x = est, y = !!sym(outcome_var), label = region)) +
    geom_point(alpha = 0.5, shape = 21, colour = "transparent", fill = "black", aes(size = sqrt(pop))) +
    geom_smooth(formula = y ~ x, method = "loess") +
    coord_cartesian(ylim = c(0, NA)) +
    geom_text_repel(max.overlaps = 20, seed = 2022) +
    labs(x = x_lab, y = outcome_lab) +
    theme_pubclean() +
    theme(legend.position = "none")
  # update x-axis
  if (slope_type == "log negative") {
    p <- p +
      scale_x_continuous(trans = compose_trans("log10", "reverse"),
                         labels = function(x) paste0("-", x))
  }
  # add % to x-axis tick labels
  if (x_percent) {
    p <- p +
      scale_x_continuous(labels = function(x) paste0(x, "%"))
  }
  # return plot
  p
}
