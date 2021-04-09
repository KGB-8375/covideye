library(shiny)
library(leaflet)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(navbarPage( "RVA CovidView", windowTitle = "RVA CovidView",
    ## DASHBOARD PAGE ##########################################################
    tabPanel("Dashboard",
        headerPanel("Our State"),
        htmlOutput("db_stats"),
        htmlOutput("db_date_ui"),
        radioButtons("db_mode", "Show", selected = "cases",
                     choices = list("Population" = "pop",
                                    "Total Cases" = "cases",
                                    "Hospitalizations" = "hosp",
                                    "Deaths" = "deaths")),
        checkboxInput("db_pop_adj", "Adjust for Population",
                      value = TRUE),
        leafletOutput("db_map"),
        headerPanel("Daily Rates"),
        htmlOutput("db_date_rng_ui"),
        plotlyOutput("db_rates"),
        headerPanel("County with Most Cases per 100k"),
        plotlyOutput("db_highest_cases"),
        headerPanel("County with Highest Daily Rate per 100k"),
        plotlyOutput("db_highest_rates")
    ),
    ## BY COUNTRY PAGE #########################################################
    tabPanel("By County",
        "This page is intentionally blank"),
    ## DEMOGRAPHICS PAGE #######################################################
    tabPanel("Demographics",
        "This page is intentionally blank"),
    ## ABOUT PAGE ##############################################################
    tabPanel("About",
        includeMarkdown("./about.md"))
))
