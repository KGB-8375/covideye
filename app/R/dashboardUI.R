# DASHBOARD UI MODULES

# Statistics
statsUI <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    fluidRow(
      align = 'center',
      column(
        width = 4,
        htmlOutput(ns("cases"))
      ),
      column(
        width = 4,
        htmlOutput(ns("hospts"))
      ),
      column(
        width = 4,
        htmlOutput(ns("deaths"))
      )
    ) 
  )
}

# Choropleth map
mapUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    mainPanel(
      width = 9,
      leafletOutput(ns("map"))
    ),
    sidebarPanel(
      width = 3,
      radioGroupButtons(
        ns("type"),
        label = "Type of Map",
        choices = list(
          "Cumulative Totals" = "total",
          "Hotspots"          = "rate"
        ),
        justified = TRUE,
        status    = "danger"
      ),
      htmlOutput(ns("date_ui")),
      radioButtons(
        ns("mode"),
        label = "Show",
        choices = list(
          "Total Cases"      = ".c",
          "Hospitalizations" = ".h",
          "Deaths"           = ".d"
        ),
        selected = ".c"
      ),
      checkboxInput(
        ns("adjust"),
        label = "Adjust for population",
        value = TRUE
      )
    )
  )
}

# State-wide daily rates
dailyRatesUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("rates"))
}

# Display county with the highest XYZ
countyHighestUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    mainPanel(
      width = 9,
      plotlyOutput(ns("chart"))
    ),
    sidebarPanel(
      width = 3,
      radioButtons(
        ns("mode"),
        label = "Show",
        choices = list(
          "Total Cases"      = ".c",
          "Hospitalizations" = ".h",
          "Deaths"           = ".d"
        ),
        selected = ".c"
      ),
      checkboxInput(
        ns("adjust"),
        label = "Adjust for population",
        value = TRUE
      ),
      radioButtons(
        ns("rank"),
        label = "Rank by",
        choices = list(
          "Total"      = "total",
          "Daily Rate" = "rate"
        ),
        selected = "rate"
      ),
      htmlOutput(ns("list"))
    )
  )
}

dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Title
    h1("The Commonwealth of Virginia"),
    hr(),
    # Statistics
    statsUI(ns("stats")),
    hr(),
    # Map
    tags$script(src = "location.js"),
    mapUI(ns("map")),
    hr(),
    # Daily Rates
    dailyRatesUI(ns("rates")),
    hr(),
    ## County with the highest cases/rates
    countyHighestUI(ns("highest"))
  )
}
