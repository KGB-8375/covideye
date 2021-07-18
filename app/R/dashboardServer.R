# DASHBOARD SERVER MODULES

statsServer <- function(id, covid.confd, pop) {
  moduleServer(
    id,
    function(input, output, session) {
      stats <- covid.confd %>%
        slice_max(date) %>%
        bind_cols(list(pop = sum(pop$pop)))
      
      fancy_num <- function(x) {
        format(
          x,
          big.mark = ",",
          scientific = FALSE
        )
      }
      
      output$cases <- renderUI({
        tagList(
          h2("Total Cases: ", fancy_num(stats$cases.t)),
          h4(fancy_num(stats$cases.c), "Confirmed | ",
             fancy_num(stats$cases.p), "Probable")
        )
      })
      
      output$hospts <- renderUI({
        tagList(
          h2("Total Hospitalizations: ", fancy_num(stats$hospts.t)),
          h4(fancy_num(stats$hospts.c), "Confirmed | ",
             fancy_num(stats$hospts.p), "Probable")
        )
      })
      
      output$deaths <- renderUI({
        tagList(
          h2("Total Deaths: ", fancy_num(stats$deaths.t)),
          h4(fancy_num(stats$deaths.c), "Confirmed | ",
             fancy_num(stats$deaths.p), "Probable")
        )
      })
    }
  )
}

mapServer <- function(id, spdf) {
  moduleServer(
    id,
    function(input, output, session) {
      spdf.min <- min(spdf$date)
      spdf.max <- max(spdf$date)
      
      # Static part of the map (bounding box)
      output$map <- renderLeaflet({
        # Set default view to bounding box of VA
        bounds <- bbox(spdf) %>%
          as.vector()
        
        leaflet(spdf) %>%
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      })
      
      # Date selector
      output$date_ui <- renderUI({
        ns <- session$ns
        
        dateInput(
          ns("date"),
          label  = "Select Date",
          format = "m/dd/yyyy",
          max    = spdf.max,
          min    = spdf.min,
          value  = spdf.max
        )
      })
      
      # Date selection (fixed)
      date_sel <- reactive({
        if(length(input$date) == 0) {
          subset(spdf, date == spdf.max)
        } else {
          subset(spdf, date == input$date)
        }
      })
      
      # Title
      title <- reactive({
        if(input$adjust) {
          switch(
            input$mode,
            pop    = "Total Population",
            cases  = "Cases per 100k",
            hospts = "Hospitalizations per 100k",
            deaths = "Deaths per 100k"
          )
        } else {
          switch(
            input$mode,
            pop    = "Total Population",
            cases  = "Total Cases",
            hospts = "Total Hospitalizations",
            deaths = "Total Deaths"
          )
        }
      })
      
      # Display value
      value <- reactive({
        if(input$adjust) {
          switch(
            input$mode,
            pop    = date_sel()$pop,
            cases  = date_sel()$cases.adj,
            hospts = date_sel()$hospts.adj,
            deaths = date_sel()$deaths.adj
          )
        } else {
          switch(
            input$mode,
            pop    = date_sel()$pop,
            cases  = date_sel()$cases,
            hospts = date_sel()$hospts,
            deaths = date_sel()$deaths
          )
        }
      })
      
      # Color range
      color <- reactive({
        switch(
          input$mode,
          pop    = "Greens",
          cases  = "YlOrRd",
          hospts = "Purples",
          deaths = "Greys"
        )
      })
      
      # Binning categories
      categories <- reactive({
        # Use Jenks Natural Breaks to find *8* bin categories
        categories <- getJenksBreaks(value(), 9)
        
        # Beautify breaks
        categories <- unique(ceiling(signif(categories, digits = 3)))
        categories[length(categories)] <- Inf
        categories[1] <- 0
        
        return(categories)
      })
      
      # Colorizing function
      color_fun <- reactive({
        colorBin(
          palette = color(),
          domain  = value(),
          bins    = categories(),
          na.color = "black"
        )
      })
      
      # Tooltip text
      tooltip <- reactive({
        paste0(
          "<b>", date_sel()$local, "</b> (", format(date_sel()$date, "%D"), ")</br>",
          "Total Cases: ",                   date_sel()$cases, "</br>",
          "Hospitalizations: ",              date_sel()$hospts, "</br>",
          "Deaths: ",                        date_sel()$deaths, "</br>",
          "Population: ",                    date_sel()$pop, "</br></br>",
          
          "Cases per 100k: ",                floor(date_sel()$cases.adj), "</br>",
          "Hospitalizations per 100k: ",     floor(date_sel()$hospts.adj), "</br>",
          "Deaths per 100k: ",               floor(date_sel()$deaths.adj), "</br>"
        ) %>% 
          lapply(htmltools::HTML)
      })
      
      # Update map
      observe({
        ns  <- session$ns
        pal <- color_fun()
        
        leafletProxy(ns("map"), data = date_sel()) %>%
          clearControls() %>%
          addLegend(
            position = "bottomleft",
            pal      = pal,
            opacity  = 0.9,
            title    = title(),
            values   = value()
          ) %>%
          clearShapes() %>%
          addPolygons(
            # Set color
            fillColor = ~pal(value()),
            fillOpacity = 1.0,
            
            # Add Borders
            stroke = T,
            color = "grey",
            weight = 0.6,
            
            # Add tooltip
            label = tooltip(),
            labelOptions = labelOptions(
              textsize   = "13px",
              direction  = "right",
              style      = list(
                "font-weight" = "normal",
                padding       = "3px 8px"
              )
            )
          )
      })
    }
  )
}

dailyRatesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$rates <- renderPlotly({
        
      })
      
      output$date_ui <- renderUI({
        ns <- session$ns
        
        dateRangeInput(
          ns("date_rng"),
          "Select Range",
          # TODO - Finish this server
        )
      })
    }
  )
}

dashboardServer <- function(id, spdf, covid.confd, pop) {
  moduleServer(
    id,
    function(input, output, session) {
      #output$stats <- renderUI({
      #wellPanel(
      #  fluidRow(
      #    column(
      #      width = 4,
      #      "placeholder"
      #    ),
      #    column(
      #      width = 4,
      #      "placeholder"
      #      
      #    ),
      #    column(
      #      width = 4,
      #      "placeholder"
      #    )
      #  )
      #)
      #)}
      
      statsServer("stats", covid.confd, pop)
        
      mapServer("map", spdf)
      
      #dailyRatesServer("rates", spdf)
      
      #output$highest_cases <- renderPlotly({
      #  
      #})
        
      #output$highest_rates <- renderPlotly({
      #  
      #})
    }
  )
} 
