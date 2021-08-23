## Retrieve all temporary data

# Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  RSocrata,
  data.table,
  dplyr,
  stringr,
  tidyr,
  zoo
)

# Create Directories
dir.create("DATA/temp", recursive = TRUE)

## Cases by locality
# https://data.virginia.gov/Government/VDH-COVID-19-PublicUseDataset-Cases/bre9-aqqr

covid.local <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json")

covid.local <- covid.local %>%
  # Fix Data Types
  transmute(
    date   = as.Date(report_date),
    fips   = as.integer(fips),
    local  = locality,
    total.c  = as.numeric(total_cases),
    total.h = as.numeric(hospitalizations),
    total.d = as.numeric(deaths)
  ) %>%
  group_by(fips) %>%
  arrange(fips, date) %>%
  mutate(
    rate.c = rollmean(total.c - lag(total.c), 7, fill = NA, align = "right"),
    rate.h = rollmean(total.h - lag(total.h), 7, fill = NA, align = "right"),
    rate.d = rollmean(total.d - lag(total.d), 7, fill = NA, align = "right")
  ) %>%
  ungroup()

fwrite(covid.local, "DATA/temp/covid_local.csv")
rm(covid.local)

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
  ) %>%
  mutate(
    cases.t  = cases.c  + cases.p,
    hospts.t = hospts.c + hospts.p,
    deaths.t = deaths.c + deaths.p
  ) %>%
  arrange(date) %>%
  mutate(
    rate.c = cases.c - lag(cases.c),
    rate.p = cases.p - lag(cases.p),
    avg    = rollmean(cases.t - lag(cases.t), 7, fill = NA)
  )
  

fwrite(covid.confd, "DATA/temp/covid_confd.csv")
rm(covid.confd)

## Cases by age group
# https://data.virginia.gov/Government/VDH-COVID-19-PublicUseDataset-Cases_By-Age-Group/uktn-mwig

covid.age <- read.socrata("https://data.virginia.gov/resource/uktn-mwig.json")

covid.age <- covid.age %>%
  # We only want age ranges from 0-9, 10-19, etc
  filter(age_group_type == "Cases" | age_group_type == "Case Age Group") %>%
  # Fix Data Types
  transmute(
    date   = as.Date(report_date),
    ages   = str_remove(age_group, " Years"),
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
rm(covid.age)

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
rm(covid.race)

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
rm(covid.sex)

## Vaccines by Locality
# https://data.virginia.gov/dataset/VDH-COVID-19-PublicUseDataset-Vaccines-DosesAdmini/28k2-x2rj

vaccn.local <- read.socrata("https://data.virginia.gov/resource/28k2-x2rj.json")

vaccn.local <- vaccn.local %>%
  # Remove unnecessary rows
  filter(fips != "Out of State" & fips != "Not Reported") %>%
  # Fix Data Types
  transmute(
    date   = as.Date(administration_date),
    fips   = as.integer(fips),
    local  = locality,
    make   = vaccine_manufacturer,
    dose   = as.numeric(dose_number),
    rate.v = as.numeric(vaccine_doses_administered)
  ) %>%
  # Remove unneeded columns
  group_by(date, fips, local, make, dose) %>%
  summarize(rate.v = sum(rate.v)) %>%
  ungroup() %>%
  # Add missing rows of data
  complete(date, nesting(fips, local, make, dose), fill = list(rate.v = 0)) %>%
  # Calculate first dose vs fully vaccinated
  mutate(
    rate.1d = if_else(dose == 1, rate.v, 0),
    rate.fv = if_else(dose == 2 | make == "J&J", rate.v, 0)
  ) %>%
  # Calculate totals
  group_by(fips, make, dose) %>%
  arrange(date) %>%
  mutate(
    total.1d = cumsum(rate.1d),
    total.fv = cumsum(rate.fv)
  ) %>%
  # Remove unneeded columns & collapse rows
  group_by(fips, local, date) %>%
  summarize(
    rate.1d  = sum(rate.1d),
    rate.fv  = sum(rate.fv),
    total.1d = sum(total.1d),
    total.fv = sum(total.fv)
  )

fwrite(vaccn.local, "DATA/temp/vaccn_local.csv")
rm(vaccn.local)
