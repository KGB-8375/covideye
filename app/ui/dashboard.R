# DASHBOARD UI

makeDashboard <- function() {
  return(
    div(
      # Title
      h1("The Commonwealth of Virginia"),
      # Statistics
      htmlOutput("db_stats"),
      # Row 1
      fluidRow( 
        # Left Edge
        column(
          width = 9,
          # Choropleth Map
          leafletOutput("db_map")
        ),
        # Right Edge
        column(
          width = 3,
          wellPanel(
            # Date picker
            htmlOutput("db_date_ui"),
            # Mode Select
            radioButtons(
              inputId  = "db_mode",
              label    = "Show",
              selected = "cases",
              choices  = list(
                "Population"       = "pop",
                "Total Cases"      = "cases",
                "Hospitalizations" = "hosp",
                "Deaths"           = "deaths"
              )
            ),
            # Population Adjust
            checkboxInput(
              inputId = "db_pop_adj",
              label   = "Adjust for Population",
              value   = TRUE
            )
          )
        )
      ),
      # Daily Rates
      h2("Daily Rates"),
      htmlOutput("db_date_rng_ui"),
      plotlyOutput("db_rates"),
      # County with the most cases
      h2("County with Most Cases (Population Adjusted)"),
      plotlyOutput("db_highest_cases"),
      # County with the highest daily rate
      h2("County with Highest Daily Rate (Population Adjusted)"),
      plotlyOutput("db_highest_rates")
    )
  )
}