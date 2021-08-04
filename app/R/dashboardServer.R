# DASHBOARD SERVER MODULES

# Statistics
statsServer <- function(id, covid.confd, pop) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare data
      stats <- covid.confd %>%
        slice_max(date) %>%
        bind_cols(list(pop = sum(pop$pop)))
      
      # Helper function, changes 10000 to 10,000
      fancy_num <- function(x) {
        format(
          x,
          big.mark   = ",",
          scientific = FALSE,
          trim       = TRUE
        )
      }
      
      # Number of cases
      output$cases <- renderUI({
        tagList(
          h2("Total Cases: ", fancy_num(stats$cases.t)),
          h4(fancy_num(stats$cases.c), "Confirmed | ",
             fancy_num(stats$cases.p), "Probable")
        )
      })
      
      # Number of hospitalizations
      output$hospts <- renderUI({
        tagList(
          h2("Total Hospitalizations: ", fancy_num(stats$hospts.t)),
          h4(fancy_num(stats$hospts.c), "Confirmed | ",
             fancy_num(stats$hospts.p), "Probable")
        )
      })
      
      # Number of deaths
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

# Choropleth map
mapServer <- function(id, local, dark_mode, navbar) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare data
      local.min <- reactive({
        if(input$type == "rate") {
          min(local$date) + 7 # 7-Day average kicks in
        } else {
          min(local$date)
        }
      })
      
      local.max <- max(local$date)
      
      # Helper function
      fancy_num <- function(x) {
        x %>%
          round() %>%
          format(
            big.mark   = ",",
            scientific = FALSE,
            trim       = TRUE
          )
      }
      
      mapDrawn <- reactiveVal(FALSE)
      
      # Draw initial map (only sets up map zoom level)
      output$map <- renderLeaflet({
        bounds <- bbox(local) %>%
          as.vector()
        
        mapDrawn(TRUE)
        
        leaflet() %>%
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      })
      
      # Date selector input
      output$date_ui <- renderUI({
        ns <- session$ns
        
        splitLayout(
          cellWidths = c("auto", "auto", "auto"),
          dateInput(
            ns("date"),
            label  = "Select Date",
            format = "m/d/yy",
            max    = local.max,
            min    = local.min(),
            value  = local.max
          ),
          actionBttn(
            ns("date_prev"),
            label = NULL,
            style = "simple",
            color = "danger",
            icon  = icon("arrow-left"),
            size  = "sm"
          ),
          actionBttn(
            ns("date_next"),
            label = NULL,
            style = "simple",
            color = "danger",
            icon  = icon("arrow-right"),
            size  = "sm"
          ),
          tags$style(
            "#dashboard-map-date_prev, #dashboard-map-date_next {
              margin-top: 32px;
            }"
          )
        )
      })
      
      # Previous Date
      observeEvent(input$date_prev, {
        req(input$date)
        
        date <- input$date
        if(date > local.min()) {
          updateDateInput(session, "date", value = date - 1)
        }
      })
      
      # Next Date
      observeEvent(input$date_next, {
        req(input$date)
        
        date <- input$date
        if(date < local.max) {
          updateDateInput(session, "date", value = date + 1)
        }
      })
      
      # Date selection
      date_sel <- reactive({
        req(input$date)
        
        local %>%
          filter(date == input$date)
      })
      
      # Generate Legend Title
      title <- reactive({
        paste(
          switch(
            input$type,
            total = "Total",
            rate  = "Daily"
          ),
          switch(
            input$mode,
            .c  = "Cases",
            .h = "Hospitalizations",
            .d = "Deaths"
          ),
          if(input$adjust) {
            "per 100k"
          }
        )
      })
      
      # Display value
      value <- reactive({
        date_sel() %>%
          select(
            ends_with(paste0(input$type, input$mode, if(input$adjust) {".adj"}))
          ) %>%
          # Column 1 -> vector
          .[[1]]
      })
      
      # Color range
      color <- reactive({
        switch(
          input$mode,
          .c = "YlOrRd",
          .h = "Purples",
          .d = "Greys"
        )
      })
      
      # Binning categories
      categories <- reactive({
        # Use Jenks Natural Breaks to find *8* bin categories
        if(input$type == "total") {
          value() %>%
            getJenksBreaks(9) %>%
            # Make them easier to read
            signif(3) %>%
            ceiling() %>%
            unique() %>%
            replace(1, 0) %>%
            replace(length(.), Inf)
        } else {
          value() %>%
            getJenksBreaks(9) %>%
            # Make them easier to read
            round() %>%
            if_else(. < 0, 0, .) %>%
            replace(1, -Inf) %>%
            replace(length(.), Inf) %>%
            unique()
        }
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
      
      tooltip <- reactive({
        if(input$type == "total") {
          sprintf(
            "<b>%s</b> (%s)</br>
            Total Cases: %s (%s / 100k)</br>
            Total Hospitalizations: %s (%s / 100k)</br>
            Total Deaths: %s (%s / 100k)</br>
            Population: %s",
            date_sel()$local, format(date_sel()$date, "%D"),
            fancy_num(date_sel()$total.c), fancy_num(date_sel()$total.c.adj),
            fancy_num(date_sel()$total.h), fancy_num(date_sel()$total.h.adj),
            fancy_num(date_sel()$total.d), fancy_num(date_sel()$total.d.adj),
            fancy_num(date_sel()$pop)
          ) %>%
            lapply(HTML)
        } else {
          sprintf(
            "<b>%s</b> (%s)</br>
            Daily Cases: %s (%s / 100k)</br>
            Daily Hospitalizations: %s (%s / 100k)</br>
            Daily Deaths: %s (%s / 100k)</br>
            Population: %s",
            date_sel()$local, format(date_sel()$date, "%D"),
            fancy_num(date_sel()$rate.c), fancy_num(date_sel()$rate.c.adj),
            fancy_num(date_sel()$rate.h), fancy_num(date_sel()$rate.h.adj),
            fancy_num(date_sel()$rate.d), fancy_num(date_sel()$rate.d.adj),
            fancy_num(date_sel()$pop)
          ) %>%
            lapply(HTML)
        }
      })
      
      # Red icon
      redIcon <- icons(
        iconUrl   = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
        shadowUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-shadow.png",
        iconWidth =   25,
        iconHeight   = 41,
        iconAnchorX  = 12,
        iconAnchorY  = 41,
        popupAnchorX = 1,
        popupAnchorY = -34,
        shadowWidth  = 41,
        shadowHeight = 41
      )
      
      # Draw location on map
      observeEvent(input$lat | input$lng, {
        req(input$geolocation)
        req(navbar() == "dashboard")
        req(mapDrawn())
        ns <- session$ns
        
        leafletProxy(
          ns("map")
        ) %>%
          clearMarkers() %>%
          addMarkers(
            lng   = input$lng,
            lat   = input$lat,
            label = HTML("<b>Your Location</b>"),
            icon  = redIcon
          )
      })
      
      # Draw full map with responsive elements
      observeEvent(value(), {
        req(navbar() == "dashboard")
        req(mapDrawn())
        ns  <- session$ns
        pal <- color_fun()
        
        leafletProxy(
          ns("map"),
          data = date_sel()
        ) %>%
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
      
      # Background changer
      observeEvent(dark_mode(), {
        req(navbar() == "dashboard")
        req(!is.null(dark_mode()))
        req(mapDrawn())
        ns <- session$ns
        
        if(dark_mode()) {
          leafletProxy(ns("map")) %>%
            clearTiles() %>%
            addProviderTiles(providers$CartoDB.DarkMatterNoLabels)
        } else {
          leafletProxy(ns("map")) %>%
            clearTiles() %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels)
        }
      })
    }
  )
}

# State-wide daily rates
dailyRatesServer <- function(id, covid.confd, dark_mode) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare data
      covid.confd.min = min(covid.confd$date) + 1
      covid.confd.max = max(covid.confd$date)
      
      covid.confd <- covid.confd %>%
        arrange(date) %>%
        mutate(
          rate.c = cases.c - lag(cases.c),
          rate.p = cases.p - lag(cases.p),
          avg    = rollmean(cases.t - lag(cases.t), 7, fill = NA)
        )
      
      # Date selector input
      output$date_ui <- renderUI({
        ns <- session$ns
        
        dateRangeInput(
          ns("date_rng"),
          "Select Range",
          format = "m/d/yy",
          start  = covid.confd.min,
          end    = covid.confd.max,
          min    = covid.confd.min,
          max    = covid.confd.max
        )
      })
      
      int_rng <- function(x) {
        x[1] %--% x[2]
      }
      
      # Target data with default oldest-newest
      rates_data <- reactive({
        req(input$date_rng)
        
        range <- input$date_rng %>%
          int_rng() %>%
          int_standardize()
        
        covid.confd %>%
          filter(
            date %within% range
          )
      })
      
      fgcolor <- reactive({
        if(dark_mode()) {
          "lightgrey"
        } else {
          "black"
        }
      })
      
      bgcolor <- reactive({
        if(dark_mode()) {
          "black"
        } else {
          "white"
        }
      })
      
      # Main plot
      output$rates <- renderPlotly({
        plot_ly(
          rates_data(),
          hovertemplate = "%{y:,.0f}",
          x             = ~date
        ) %>% add_lines (
          # Average line
          line    = list(
            color = fgcolor(),
            shape = "spline"
          ),
          y       = ~avg,
          name    = "Total (7-Day Average)"
          # Bar 1
        ) %>% add_trace (
          type  = 'bar',
          y     = ~rate.c,
          name  = "Confirmed",
          color = I("darkred")
          # Bar 2
        ) %>% add_trace (
          type  = 'bar',
          y     = ~rate.p,
          name  = "Probable",
          color = I("red")
          # Extra settings
        ) %>% layout (
          title  = "Daily Virginia COVID-19 Rates",
          legend = list(
            x           = 0,
            y           = 1,
            bgcolor     = "transparent",
            bordercolor = "transparent"
          ),
          xaxis = list(
            title          = "Date",
            showgrid       = FALSE,
            fixedrange     = TRUE,
            showspikes     = TRUE,
            spikethickness = 2,
            spikedash      = 'dot',
            spikecolor     = "darkgrey",
            spikemode      = 'across'
          ),
          yaxis = list(
            title      = "Cases",
            showgrid   = FALSE,
            fixedrange = TRUE
          ),
          hoverlabel = list(
            bordercolor = fgcolor(),
            bgcolor     = bgcolor()
          ),
          barmode       = 'group',
          hovermode     = 'x unified',
          hoverdistance = 1,
          spikedistance = 1000,
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          font = list(
            color = fgcolor()
          )
        ) %>% config (
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        )
      }) %>%
        bindCache(
          dark_mode(),
          input$date_rng
        )
    }
  )
}

# Display county with the highest XYZ
countyHighestServer <- function(id, covid.local, covid.confd, dark_mode) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare data
      pop <- covid.local %>%
        slice_max(date) %>%
        select(pop) %>%
        sum()
      
      covid.confd <- covid.confd %>%
        arrange(date) %>%
        # Systematically rename
        transmute(
          date        = date,
          total.c     = cases.t,
          total.h     = hospts.t,
          total.d     = deaths.t
        ) %>%
        # Calculate the rates
        mutate(
          rate.c = rollmean(total.c - lag(total.c), 7, fill = NA, align = "right"),
          rate.h = rollmean(total.h - lag(total.h), 7, fill = NA, align = "right"),
          rate.d = rollmean(total.d - lag(total.d), 7, fill = NA, align = "right")
        ) %>%
        # Adjust for population
        mutate(
          total.c.adj = total.c * 100000 / pop,
          total.h.adj = total.h * 100000 / pop,
          total.d.adj = total.d * 100000 / pop,
          rate.c.adj  = rate.c  * 100000 / pop,
          rate.h.adj  = rate.h  * 100000 / pop,
          rate.d.adj  = rate.d  * 100000 / pop
        )
      
      target <- reactive({
        paste0(
          input$rank, input$mode, if(input$adjust) {".adj"}
        )
      })
      
      # Select target metric
      value.co <- reactive({
        covid.local %>%
          select(
            date,
            local,
            target.co = ends_with(target())
          )
      })
      
      # Select target metric for state
      value.va <- reactive({
        covid.confd %>%
          select(
            date,
            target.va = ends_with(target())
          )
      })
      
      # Rank values for selection input
      rank <- reactive({
        value.co() %>%
          slice_max(date) %>%
          arrange(desc(target.co))
      })
      
      # County selection, ranked in descending order
      output$list <- renderUI({
        ns <- session$ns
        
        selectInput(
          ns("list"),
          "Select County",
          rank()$local
        )
      }) %>%
        bindCache(
          target()
        )
      
      # Selected county's data
      selection <- reactive({
        req(input$list)
        
        value.co() %>%
          filter(local == input$list) %>%
          inner_join(value.va(), by = "date")
      })
      
      title_plot <- reactive({
        req(input$list)
        
        paste(
          switch(
            input$rank,
            "total" = "Total",
            "rate"  = "Daily"
          ),
          switch(
            input$mode,
            ".c" = "Cases",
            ".h" = "Hospitalizations",
            ".d" = "Deaths"
          ),
          "in",
          input$list,
          "vs. State",
          if(input$adjust) {
            "Average (Population Adjusted)"
          } else {
            "Total"
          }
        )
      })
      
      title_yaxis <- reactive({
        paste(
          switch(
            input$mode,
            ".c" = "Cases",
            ".h" = "Hospitalizations",
            ".d" = "Deaths"
          ),
          if(input$adjust) {
            "per 100k"
          }
        )
      })
      
      title_va <- reactive({
        paste(
          "VA",
          if(input$adjust) {
            "Average"
          } else {
            "Total"
          }
        )
      })
      
      fgcolor <- reactive({
        if(dark_mode()) {
          "lightgrey"
        } else {
          "black"
        }
      })
      
      bgcolor <- reactive({
        if(dark_mode()) {
          "black"
        } else {
          "white"
        }
      })

      output$chart <- renderPlotly({
        req(input$list)
        
        plot_ly(
          selection(),
          x     = ~date,
          hovertemplate = "%{y:,.0f}"
        ) %>% add_lines (
          # County cases
          y    = ~target.co,
          name = input$list,
          line = list(
            color = "red",
            shape = "spline"
          )
        ) %>% add_lines (
          # VA average
          y    = ~target.va,
          name = title_va(),
          line = list(
            color = fgcolor(),
            shape = "spline"
          )
        ) %>% layout (
          # Setup
          title = title_plot(),
          legend = list(
            x           = 0,
            y           = 1,
            bgcolor     = "transparent",
            bordercolor = "transparent"
          ),
          yaxis = list(
            title      = title_yaxis(),
            showgrid   = FALSE,
            fixedrange = TRUE
          ),
          xaxis = list(
            title          = "Date",
            showgrid       = FALSE,
            fixedrange     = TRUE,
            showspikes     = TRUE,
            spikethickness = 2,
            spikedash      = 'dot',
            spikecolor     = "darkgrey",
            spikemode      = 'across'
          ),
          hoverlabel = list(
            bordercolor = fgcolor(),
            bgcolor     = bgcolor()
          ),
          hovermode     = 'x unified',
          hoverdistance = 1,
          spikedistance = 1000,
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          font = list(
            color = fgcolor()
          )
        ) %>% config (
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        )
      }) %>% bindCache(
        dark_mode(),
        target(),
        input$list
      )
    }
  )
}

dashboardServer <- function(id, local, covid.confd, pop, dark_mode, navbar) {
  moduleServer(
    id,
    function(input, output, session) {
      statsServer("stats", covid.confd, pop)
      mapServer("map", local, dark_mode, navbar)
      dailyRatesServer("rates", covid.confd, dark_mode)
      countyHighestServer("highest", local@data, covid.confd, dark_mode)
    }
  )
} 
