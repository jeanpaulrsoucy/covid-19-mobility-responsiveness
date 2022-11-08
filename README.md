# Characterizing responsiveness to the COVID-19 pandemic in the United States and Canada using mobility data

## Purpose of this repository

This repository contains all data and code necessary to reproduce the analyses presented in the manuscript "Characterizing responsiveness to the COVID-19 pandemic in the United States and Canada using mobility data" by Soucy et al. Note that the included scripts may produce additional tables and figures beyond those directly presented in the manuscript.

## Requirements

All code is written in the [R programming language](https://www.r-project.org/). The easiest way to run it is to use the [RStudio](https://rstudio.com/) IDE. An `.Rproj` file is included with this repository for ease of use with RStudio. The analysis script should run with any modern version of R.

The R packages required to reproduce the analyses are listed at the top of each script (and the common functions script, `funs.R`). They must be installed using `install.packages` or similar functionality within RStudio prior to running each script.

Additionally, the analysis script requires [ImageMagick](https://imagemagick.org/index.php) to be installed as part of the figure generation process (used by the package [`magick`](https://cran.r-project.org/web/packages/magick/vignettes/intro.html). The binaries for this program should be installed automatically when downloading the package from CRAN on Windows and MacOS but need to be installed manually on Linux.

## Reproducing tables and figures

Raw data have already been downloaded (`1_download-data.R`) and processed (`2_prepare-data.R`), with the results being available in the `raw`, `pop` and `data` directories, so rerunning them is unnecessary. Some data referenced in `1_download-data.R` were downloaded and processed manually (see comments in script for more details). Run `3_analysis.R` to create the output tables and figures.

## Tables

- [Table 1](tables/meta_summary_full.csv)
- [Table S1](tables/meta_summary_full.csv)
- [Table S2](tables/meta_summary_intonly.csv)

## Figures

- [Figure 1](figures/descriptive_case_rate_country.png)
- [Figure 2](figures/meta_country_rr_rising_full_1.png)
- [Figure 3](figures/deaths_period_regs_rr_rising_full_1.png)
- [Figure S1](figures/deaths_period_regs_rr_avg.png)
- [Figure S2](figures/meta_country_rr_rising_intonly_1.png)
- [Figure S3](figures/deaths_period_regs_rr_rising_intonly_1.png)
- [Figure S4](figures/descriptive_case_rate_atlantic.png)

## Notes

The datasets, tables and figures in this repository were produced in the R environment specified in [`sessionInfo.txt`](sessionInfo.txt).
