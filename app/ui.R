library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)

# Scripts
source("ui/dashboard.R",    local = TRUE)
source("ui/byCounty.R",     local = TRUE)
source("ui/demographics.R", local = TRUE)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
        title       = "CovidEye",
        windowTitle = "CovidEye",
        theme       = shinytheme("cosmo"),
        
        # Dashboard
        tabPanel(
            title = "Dashboard",
            makeDashboard()
        ),
        # By County
        tabPanel(
            title = "By County",
            makeByCounty()
        ),
        # Demographics
        tabPanel(
            title = "Demographics",
            makeDemographics()
        ),
        # About
        tabPanel(
            title = "About",
            includeMarkdown("./about.md")
        ),
        # Disclaimer
        tabPanel(
            title = "Disclaimer",
            includeMarkdown("./disclaimer.md")
        )
    )
)
