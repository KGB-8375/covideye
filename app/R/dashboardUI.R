# DASHBOARD UI MODULES

# Statistics
statBuilder <- function(name, display) {
  tagList(
    h2(paste0("Total ", display, ": "), textOutput(paste0(name, ".t"), inline = TRUE)),
    h4(textOutput(paste0(name, ".c"), inline = TRUE), "Confirmed | ",
       textOutput(paste0(name, ".p"), inline = TRUE), "Probable")
  )
}

statsUI <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    fluidRow(
      align = 'center',
      column(
        width = 4,
        statBuilder(ns("cases"), "Cases")
      ),
      column(
        width = 4,
        statBuilder(ns("hospts"), "Hospitilizations")
      ),
      column(
        width = 4,
        statBuilder(ns("deaths"), "Deaths")
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
      leafletOutput(ns("map")) %>%
        withGraphSpinner()
    ),
    sidebarPanel(
      width = 3,
      hotspotInputUI(ns("type"), "Type of Map"),
      dateInputUI(ns("date")),
      targetInputUI(ns("target"))
    )
  )
}

# State-wide daily rates
dailyRatesUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("rates")) %>%
    withGraphSpinner()
}

# Display county with the highest XYZ
countyHighestUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    mainPanel(
      width = 9,
      plotlyOutput(ns("chart")) %>%
        withGraphSpinner()
    ),
    sidebarPanel(
      width = 3,
      hotspotInputUI(ns("rank"), "Rank By"),
      targetInputUI(ns("mode")),
      countyInputUI(ns("county"))
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
