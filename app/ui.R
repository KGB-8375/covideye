## Main client-side app

# Libraries
library(shiny)
library(leaflet)
library(plotly)
library(bslib)
library(shinyWidgets)
library(htmlwidgets)
library(htmltools)

# Fancier navbarPage
navbarPageWithInputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form", inputs)
    navbar[[4]][[1]]$children[[1]] <- tagAppendChild(
        navbar[[4]][[1]]$children[[1]], form
    )
    navbar
}

# Main UI for website
navbarPageWithInputs(
    title       = "CovidEye",
    windowTitle = "CovidEye",
    theme       = light,
    
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
    # Demographics
    tabPanel(
        title = "Demographics"
    #    demographicsUI("demographics")
    ),
    # About
    tabPanel(
        title = "About",
        includeMarkdown("./about.md")
    ),
    # Dark Mode
    inputs = tagList(
        switchInput(
        "dark_mode",
        size = "small",
        onLabel = '<i class="fa fa-moon"></i>',
        offLabel = '<i class="fa fa-sun"></i>',
        offStatus = "warning",
        onStatus = "info",
        handleWidth = "20px"
        ),
        tags$style(".bootstrap-switch-id-dark_mode {position: absolute; right: 20px; top: 12px; }")
    ),
    # Disclaimer
    footer = tagList(
        hr(),
        wellPanel(includeText("disclaimer.txt"), style = "font-size:8pt")
    )
)
