##### 1: Download data #####
##### Characterizing responsiveness to the COVID-19 pandemic in the United States and Canada using mobility data #####
##### Soucy et al. #####
##### Script by Jean-Paul R. Soucy #####
##### https://github.com/jeanpaulrsoucy/covid-19-mobility-responsiveness #####

# load libraries
library(curl)

# create raw data directory
dir.create("raw", showWarnings = FALSE)

# download population data

## Canada provincial populations, 2021 July 1
# formatted file stored as pop/pop-can.csv
# https://open.canada.ca/data/en/dataset/ecdee020-5919-4996-8d3d-c3df75f50ca0/resource/9b32ea44-2468-4af1-be66-2031d63cbf52

## USA state populations, 2021 July 1
# formatted file stored as pop/pop-usa.csv
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-total.html#par_textimage_1873399417

## USA census region data
# https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv

# download case and death data

## CDC cases and deaths by state
# https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36/data
curl_download("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD", "raw/cdc.csv")

## PHAC cases and deaths by province
# https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html
# note that the format and content of this dataset have changed from when it was originally downloaded
# curl_download("https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv", "raw/phac.csv")

# download Google mobility data
# https://www.google.com/covid19/mobility/
curl_download("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip", "raw/mobility.zip")
unzip("raw/mobility.zip", files = c(
  "2020_CA_Region_Mobility_Report.csv", "2021_CA_Region_Mobility_Report.csv",
  "2020_US_Region_Mobility_Report.csv", "2021_US_Region_Mobility_Report.csv"),
  exdir = "raw")
unlink("raw/mobility.zip")
