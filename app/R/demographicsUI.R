# DEMOGRAPHICS UI

# Input
inputUI <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    htmlOutput(ns("date_ui")),
    radioButtons(
      inputId = ns("mode"),
      label = "Show",
      selected = "cases",
      choices = list(
        "Cases" = "cases",
        "Hospitalizations" = "hospts",
        "Deaths" = "deaths"
      )
    ),
    checkboxInput(
      inputId = ns("pop_adj"),
      label = "Adjust for Population",
      value = TRUE
    )
  )
}

# Age
ageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Age Groups"),
    plotlyOutput(ns("age"))
  )
}

# Sex
sexUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Sex"),
    plotlyOutput(ns("sex"))
  )
}

# Race
raceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Race"),
    plotlyOutput(ns("race"))
  )
}

# MAIN UI
demographicsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("Who COVID-19 is affecting the most"),
    hr(),
    fluidRow(
      align = 'center',
      column(
        width = 4,
        ageUI(ns("age"))
      ),
      column(
        width = 4,
        sexUI(ns("sex"))
      ),
      column(
        width = 4,
        raceUI(ns("race"))
      )
    ),
    fluidRow(
      column(width = 4),
      column(
        width = 4,
        inputUI(ns("input"))
      ),
      column(width = 4)
    )
  )
}
