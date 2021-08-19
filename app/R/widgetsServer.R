# WIDGETS SERVER MODULES
## Widgets shared across multiple modules

hotspotInputServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(reactive(input$type))
    }
  )
}

dateInputServer <- function(id, min_date, max_date) {
  moduleServer(
    id,
    function(input, output, session) {
      # Set minimum date
      observeEvent(min_date(), {
        updateAirDateInput(session, "date", options = list("minDate" = min_date()))

        if(is.Date(input$date) && input$date < min_date()) {
          updateAirDateInput(session, "date", value = min_date())
        }
      })

      # Set maximum date
      observeEvent(max_date(), {
        updateAirDateInput(session, "date", options = list("maxDate" = max_date()))

        if(is.Date(input$date) && input$date > max_date()) {
          updateAirDateInput(session, "date", value = max_date())
        }
      })

      # Previous Date
      observeEvent(input$prev_date, {
        if(input$date > min_date()) {
          updateAirDateInput(session, "date", value = input$date - 1)
        }
      })

      # Next Date
      observeEvent(input$next_date, {
        if(input$date < max_date()) {
          updateAirDateInput(session, "date", value = input$date + 1)
        }
      })

      # Set date if none is detected
      observe({
        if(!is.Date(input$date)) {
          updateAirDateInput(session, "date", value = max_date())
        }
      })
      
      return(reactive(input$date))
    }
  )
}

targetInputServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(list(
        mode   = reactive(input$mode),
        adjust = reactive(input$adjust)
      ))
    }
  )
}
