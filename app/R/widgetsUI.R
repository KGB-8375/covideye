# WIDGETS UI MODULES
## Widgets shared across multiple modules

hotspotInputUI <- function(id, label) {
  ns <- NS(id)
  
  radioGroupButtons(
    ns("type"),
    label,
    status    = "danger",
    justified = TRUE,
    choices   = list(
      "Cumulative Totals" = "total",
      "Hotspots"          = "rate"
    ),
    selected  = "rate"
  )
}

dateInputUI <- function(id) {
  ns <- NS(id)
  
  splitLayout(
    cellWidths = c("auto"),
    airDatepickerInput(
      ns("date"),
      "Select Date",
      dateFormat  = "m/d/yy",
      addon       = "none",
      todayButton = TRUE
    ),
    actionBttn(
      ns("prev_date"),
      NULL,
      style = "simple",
      color = "danger",
      icon  = icon("arrow-left"),
      size  = "sm"
    ),
    actionBttn(
      ns("next_date"),
      NULL,
      style = "simple",
      color = "danger",
      icon  = icon("arrow-right"),
      size  = "sm"
    ),
    tags$style(
      paste0("#", ns("prev_date"), ", #", ns("next_date"), "{margin-top: 32px;}")
    )
  )
}

targetInputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    prettyRadioButtons(
      ns("mode"),
      "Show",
      choices = list(
        "Cases"            = "c",
        "Hospitilizations" = "h",
        "Deaths"           = "d"
      ),
      selected = "c",
      status   = "danger"
    ),
    prettyCheckbox(
      ns("adjust"),
      "Adjust for population",
      TRUE,
      status = "danger",
      shape  = "curve"
    )
  )
}

countyInputUI <- function(id) {
  ns <- NS(id)
  
  pickerInput(
    ns("county"),
    "Select County",
    options = list(
      `live-search` = TRUE,
      `dropup-auto` = FALSE,
      `style`       = "btn-outline-danger"
    ),
    choices = c("Placeholder")
  )
}
