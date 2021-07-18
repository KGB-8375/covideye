## Main server-side app

# Libraries
library(shiny)      # Building interactive websites
library(data.table) # Quickly read from .csv files
library(rgdal)      # Reading spatial data files
library(sp)         # Working with spatial data
library(leaflet)    # Graphing interactive maps
library(dplyr)      # Arranging & modifying data
library(spdplyr)    # Use dplyr verbs on spatial data
library(zoo)        # Calculating a rolling mean
library(plotly)     # Graphing interactive plots
library(BAMMtools)  # Jenks breaks

# Read cached data tables
covid.local <- fread("DATA/temp/covid_local.csv")
covid.confd <- fread("DATA/temp/covid_confd.csv")
covid.age   <- fread("DATA/temp/covid_age.csv")
covid.race  <- fread("DATA/temp/covid_race.csv")
covid.sex   <- fread("DATA/temp/covid_sex.csv")
pop         <- fread("DATA/pop.csv")
spdf        <- readOGR(
    dsn   = "./DATA/shapefile",
    layer = "cb_2019_us_county_500k"
)

# Prepare data

## SPDF (Spatial data file)
pop.local <- pop %>%
    group_by(fips) %>%
    summarize(pop = sum(pop))

spdf <- spdf %>%
    filter(STATEFP == "51") %>%
    transmute(fips = GEOID) %>%
    merge(covid.local, duplicateGeoms = TRUE) %>%
    merge(pop.local,   duplicateGeoms = TRUE) %>%
    mutate(
        cases.adj  = cases  * 100000 / pop,
        hospts.adj = hospts * 100000 / pop,
        deaths.adj = deaths * 100000 / pop
    )

rm(covid.local, pop.local)

## Confirmed cases
covid.confd <- covid.confd %>%
    arrange(date) %>%
    mutate(
        cases.t  = cases.c  + cases.p,
        hospts.t = hospts.c + hospts.p,
        deaths.t = deaths.c + deaths.p
    ) %>%
    mutate(
        rate.c = cases.c - lag(cases.c),
        rate.p = cases.p - lag(cases.p),
        rate.t = cases.t - lag(cases.t)
    ) %>%
    mutate(
        avg = rollmean(rate.t, 7, fill = NA)
    )

# Main server functionality for website
shinyServer(function(input, output, session) ({
    dashboardServer("dashboard", spdf, covid.confd, pop)
    byCountyServer("byCounty")
    #demographicsServer("demographics")
})
)
