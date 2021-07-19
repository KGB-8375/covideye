## Main client-side app

# Libraries
library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)

# Main UI for website
navbarPage(
    title       = "CovidEye",
    windowTitle = "CovidEye",
    theme       = shinytheme("cosmo"),
    
    # Dashboard
    tabPanel(
        title = "Dashboard",
        dashboardUI("dashboard")
    ),
    # By County
    tabPanel(
        title = "By County",
        byCountyUI("byCounty")
    ),
    ## Demographics
    #tabPanel(
    #    title = "Demographics",
    #    demographicsUI("demographics")
    #),
    # About
    tabPanel(
        title = "About",
        includeMarkdown("./about.md")
    ),
    # Disclaimer
    footer = tagList(
        hr(),
        wellPanel(includeText("disclaimer.txt"), style = "font-size:8pt")
    )
)
