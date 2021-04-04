library(shiny)
library(leaflet)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(navbarPage( "RVA CovidView", windowTitle = "RVA CovidView",
    ## DASHBOARD PAGE ##########################################################
    tabPanel("Dashboard",
        headerPanel("Our State"),
        htmlOutput("db_date_ui"),
        leafletOutput("db_map"),
        headerPanel("Daily Rates"),
        htmlOutput("db_date_rng_ui"),
        plotlyOutput("db_rates")
        #headerPanel("County with Most Cases"),
        #plotOutput("db_highest_cases"),
        #headerPanel("County with Highest Daily Rate"),
        #plotOutput("db_highest_rate"),
        #headerPanel("County with Largest Jump in Cases"),
        #plotOutput("db_highest_jump")
    ),
    ## BY COUNTRY PAGE #########################################################
    tabPanel("By County"),
    ## DEMOGRAPHICS PAGE #######################################################
    tabPanel("Demographics"),
    ## ABOUT PAGE ##############################################################
    tabPanel("About")
))
