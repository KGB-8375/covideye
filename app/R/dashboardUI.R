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
      htmlOutput(ns("date_ui")),
      radioButtons(
        ns("mode"),
        label = "Show",
        choices = list(
          "Population"       = "pop",
          "Total Cases"      = "cases",
          "Hospitalizations" = "hospts",
          "Deaths"           = "deaths"
        ),
        selected = "cases"
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
  
  tagList(
    plotlyOutput(ns("rates")),
    fluidRow(
      align = "center",
      column(
        width = 4
      ),
      column(
        width = 4,
        inputPanel(
          htmlOutput(ns("date_ui"))
        )
      ),
      column(
        width = 4
      )
    )
  )
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
        selected = "total"
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
    mapUI(ns("map")),
    hr(),
    # Daily Rates
    dailyRatesUI(ns("rates")),
    hr(),
    ## County with the highest cases/rates
    countyHighestUI(ns("highest"))
  )
}
