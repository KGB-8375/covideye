library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage( "RVA CovidView", windowTitle = "RVA CovidView",
                    theme = shinytheme("cosmo"),
    ## DASHBOARD PAGE ##########################################################
    tabPanel("Dashboard",
        tags$h1("The Commonwealth of Virginia"),
        htmlOutput("db_stats"),
        fluidRow(
            column(9, leafletOutput("db_map")),
            column(3, inputPanel(
                htmlOutput("db_date_ui"),
                radioButtons("db_mode", "Show", selected = "cases",
                             choices = list("Population" = "pop",
                                            "Total Cases" = "cases",
                                            "Hospitalizations" = "hosp",
                                            "Deaths" = "deaths")),
                checkboxInput("db_pop_adj", "Adjust for Population",
                              value = TRUE))
            )
        ),
        tags$h2("Daily Rates"),
        htmlOutput("db_date_rng_ui"),
        plotlyOutput("db_rates"),
        tags$h2("County with Most Cases (Population Adjusted)"),
        plotlyOutput("db_highest_cases"),
        tags$h2("County with Highest Daily Rate (Population Adjusted)"),
        plotlyOutput("db_highest_rates")
    ),
    ## BY COUNTRY PAGE #########################################################
    tabPanel("By County",
        "Abubu"),
    ## DEMOGRAPHICS PAGE #######################################################
    tabPanel("Demographics",
        tags$h1("Who COVID-19 is affecting the most"),
        htmlOutput("demo_date_ui"),
        radioButtons("demo_mode","Show", selected = "cases",
            choices = list("Cases" = "cases",
                           "Hospitilizations" = "hosp",
                           "Deaths" = "deaths")),
        checkboxInput("demo_pop_adj", "Adjust for Population",
                      value = TRUE),
        tags$h2("Age Groups"),
        plotlyOutput("demo_age"),
        tags$h2("Race"),
        plotlyOutput("demo_race"),
        tags$h2("Sex"),
        plotlyOutput("demo_sex")
    ),
    ## ABOUT PAGE ##############################################################
    tabPanel("About",
        includeMarkdown("./about.md")),
    tabPanel("Disclaimer",
        includeMarkdown("./disclaimer.md"))
))
