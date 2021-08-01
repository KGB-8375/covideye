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
    plotlyOutput(
      ns("sex"),
      width = '50%'
    )
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
      column(
        width = 6,
        #Input
        inputUI(ns("input"))
      ),
      column(
        width = 6,
        align = 'center',
        #Age
        ageUI(ns("age"))
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 6,
        align = 'center',
        #Sex
        sexUI(ns("sex"))
      ),
      column(
        width = 6,
        align = 'center',
        #Race
        raceUI(ns("race"))
      )
    )
  )
}
