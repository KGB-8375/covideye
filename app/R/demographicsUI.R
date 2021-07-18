# DEMOGRAPHICS UI

demographicsUI <- function(id) {
  ns <- NS("id")
  
  tagList(
    # Title
    h1("Who COVID-19 is affecting the most"),
    # Top Row
    fluidRow(
      # Top Left Corner
      column(
        width = 6,
        wellPanel(
          # Date selector
          htmlOutput(ns("date_ui")),
          # Mode selector
          radioButtons(
            inputId  = ns("mode"),
            label    = "Show", 
            selected = "cases",
            choices  = list(
              "Cases"            = "cases",
              "Hospitilizations" = "hosp",
              "Deaths"           = "deaths"
            )
          ),
          # Population Adjust
          checkboxInput(
            inputId = ns("pop_adj"),
            label   = "Adjust for Population",
            value   = TRUE
          )
        )
      ),
      # Top Right Corner
      column(
        width = 6,
        align = 'center',
        # Age stats
        h2("Age Groups"),
        plotlyOutput(ns("age"))
      )
    ),
    # Bottom Row
    fluidRow(
      # Bottom Left Corner
      column(
        width = 6,
        align = 'center',
        # Sex stats
        h2("Sex"),
        plotlyOutput(
          ns("sex"),
          width = '50%'
        )
      ),
      # Bottom Right Corner
      column(
        width = 6,
        align = 'center',
        # Race stats
        h2("Race"),
        plotlyOutput(ns("race"))
      )
    )
  )
}