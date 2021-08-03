# Shared between UI and Server

# Libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(thematic)
library(data.table)
library(rgdal)
library(sp)
library(dplyr)
library(spdplyr)
library(lubridate)
library(zoo)
library(BAMMtools)
library(plotly)
library(leaflet)
library(htmltools)

light <- bs_theme(bootswatch = "cosmo")
dark  <- bs_theme(
  bootswatch = "cosmo",
  bg = "#161616",
  fg = "#D5D5D7",
  danger = "#E12952")