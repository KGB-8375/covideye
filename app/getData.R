## Retrieve all static data

# Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  RSocrata,
  dplyr,
  data.table
)

# Create Directories
dir.create("DATA")

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
rm(pop)

## Shapefile
# https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2019.html

temp <- tempfile()
download.file("https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_county_500k.zip", temp)
unzip(temp, exdir = "DATA/shapefile")
rm(temp)
