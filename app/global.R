# Shared between UI and Server

# Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny,
  shinyjs,
  shinyWidgets,
  bslib,
  thematic,
  data.table,
  rgdal,
  sp,
  dplyr,
  spdplyr,
  lubridate,
  zoo,
  BAMMtools,
  plotly,
  leaflet,
  htmltools
)

# Theming
light <- bs_theme(
  bootswatch = "cosmo"
)

dark  <- bs_theme(
  bootswatch = "cosmo",
  bg = "#161616",
  fg = "#D5D5D7"
)

thematic_shiny()
