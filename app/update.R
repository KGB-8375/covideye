## Update all temporary data, including wrangling

# Libraries
library(RSocrata)   # Reading data from Socrata sites (VDH)
library(dplyr)      # Arranging & modifying data
library(data.table) # Write data frame to .csv

## Cases by locality
# https://data.virginia.gov/Government/VDH-COVID-19-PublicUseDataset-Cases/bre9-aqqr

covid.local <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json")

covid.local <- covid.local %>%
  # Fix Data Types
  transmute(
    date   = as.Date(report_date),
    fips   = fips,
    local  = locality,
    cases  = as.numeric(total_cases),
    hospts = as.numeric(hospitalizations),
    deaths = as.numeric(deaths)
  )

fwrite(covid.local, "DATA/temp/covid_local.csv")

## Cases by confirmation
# https://data.virginia.gov/Government/VDH-COVID-19-PublicUseDataset-Cases_By-Confirmatio/uqs3-x7zh

covid.confd <- read.socrata("https://data.virginia.gov/resource/uqs3-x7zh.json")

covid.confd <- covid.confd %>%
  # Fix Data types
  transmute(
    date   = as.Date(report_date),
    status = case_status,
    cases  = as.numeric(number_of_cases),
    hospts = as.numeric(number_of_hospitalizations),
    deaths = as.numeric(number_of_deaths)
  ) %>%
  # Split by confirmed
  group_split(
    status,
    .keep = FALSE
  ) %>%
  # Recombine as columns
  left_join(
    x = .[[1]],
    y = .[[2]],
    by = "date",
    copy = TRUE,
    suffix = c(".c", ".p")
  )

fwrite(covid.confd, "DATA/temp/covid_confd.csv")

## Cases by age group
# https://data.virginia.gov/Government/VDH-COVID-19-PublicUseDataset-Cases_By-Age-Group/uktn-mwig

covid.age <- read.socrata("https://data.virginia.gov/resource/uktn-mwig.json")

covid.age <- covid.age %>%
  # Fix Data Types
  transmute(
    date   = as.Date(report_date),
    ages   = age_group,
    cases  = as.numeric(number_of_cases),
    hospts = as.numeric(number_of_hospitalizations),
    deaths = as.numeric(number_of_deaths)
  ) %>%
  # Sum across dates & age groups (we don't need health district separation)
  group_by(
    ages,
    date
  ) %>%
  summarize(
    cases  = sum(cases),
    hospts = sum(hospts),
    deaths = sum(deaths)
  )

fwrite(covid.age, "DATA/temp/covid_age.csv")

## Cases by race
# https://data.virginia.gov/Government/VDH-COVID-19-PublicUseDataset-Cases_By-Race-Ethnic/9sba-m86n

covid.race <- read.socrata("https://data.virginia.gov/resource/9sba-m86n.json")

covid.race <- covid.race %>%
  # Fix Data Types
  transmute(
    date   = as.Date(report_date),
    race   = race_and_ethnicity,
    cases  = as.numeric(number_of_cases),
    hospts = as.numeric(number_of_hospitalizations),
    deaths = as.numeric(number_of_deaths)
  ) %>%
  # Sum across dates & races (we don't need health district separation)
  group_by(
    race,
    date
  ) %>%
  summarize(
    cases  = sum(cases),
    hospts = sum(hospts),
    deaths = sum(deaths)
  )

fwrite(covid.race, "DATA/temp/covid_race.csv")

## Cases by sex
# https://data.virginia.gov/Government/VDH-COVID-19-PublicUseDataset-Cases_By-Sex/tdt3-q47w

covid.sex <- read.socrata("https://data.virginia.gov/resource/tdt3-q47w.json")

covid.sex <- covid.sex %>%
  # Fix Data Types
  transmute(
    date   = as.Date(report_date),
    sex    = sex,
    cases  = as.numeric(number_of_cases),
    hospts = as.numeric(number_of_hospitalizations),
    deaths = as.numeric(number_of_deaths)
  ) %>%
  # Sum across dates & races (we don't need health district separation)
  group_by(
    sex,
    date
  ) %>%
  summarize(
    cases  = sum(cases),
    hospts = sum(hospts),
    deaths = sum(deaths)
  )

fwrite(covid.sex, "DATA/temp/covid_sex.csv")
