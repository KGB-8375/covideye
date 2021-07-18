# DASHBOARD UI MODULES

mapUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 9,
      leafletOutput(ns("map"))
    ),
    column(
      width = 3,
      wellPanel(
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
  )
}

dailyRatesUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 9,
      plotlyOutput("rates")
    ),
    column(
      width = 3,
      wellPanel(
        htmlOutput(ns("date_ui")),
      )
    )
  )
}

dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Title
    h1("The Commonwealth of Virginia"),
    # Statistics
    htmlOutput(ns("stats")),
    # Map
    mapUI(ns("map")),
    # Daily Rates
    dailyRatesUI(ns("rates")),
    # County with the highest cases
    plotlyOutput(ns("highest_cases")),
    # County with the highest daily rate
    plotlyOutput(ns("highest_rates"))
  )
}
