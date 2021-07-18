## Retrieve all static data

# Libraries
library(RSocrata)   # Reading data from Socrata sites (VDH)
library(dplyr)      # Arranging & modifying data
library(data.table) # Write data frame to .csv

## Population
# https://data.virginia.gov/dataset/VDH-PublicUseDataset-NCHS-Population/5s4f-hthh

pop <- read.socrata("https://data.virginia.gov/resource/5s4f-hthh.json")

pop <- pop %>%
  slice_max(year) %>%
  transmute(
    fips  = fips,
    local = locality,
    ages  = age_group,
    sex   = sex,
    race  = race_and_ethnicity,
    pop   = as.numeric(population_estimate)
  ) %>%
  group_by(
    local,
    ages,
    sex,
    race
  ) %>%
  summarize(
    fips = fips,
    pop = sum(pop)
  )

fwrite(pop, "DATA/pop.csv")
