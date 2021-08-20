# DEMOGRAPHICS UI

# Age
ageUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("age"))
}

# Sex
sexUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("sex"))
}

# Race
raceUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("race"))
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
        wellPanel(
          dateInputUI(ns("date")),
          targetInputUI(ns("target"))
        )
      ),
      column(width = 4)
    )
  )
}
