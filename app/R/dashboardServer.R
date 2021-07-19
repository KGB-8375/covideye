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
          big.mark = ",",
          scientific = FALSE
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
mapServer <- function(id, local) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare data
      local.min <- min(local$date)
      local.max <- max(local$date)
      
      # Draw initial map (only sets up map zoom level)
      output$map <- renderLeaflet({
        bounds <- bbox(local) %>%
          as.vector()
        
        leaflet(local) %>%
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      })
      
      # Date selector input
      output$date_ui <- renderUI({
        ns <- session$ns
        
        dateInput(
          ns("date"),
          label  = "Select Date",
          format = "m/dd/yyyy",
          max    = local.max,
          min    = local.min,
          value  = local.max
        )
      })
      
      # Date selection (missing info defaults to newest)
      date_sel <- reactive({
        req(input$date)
        
        subset(local, date == input$date)
      })
      
      # Generate Title
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
      
      # Draw full map with responsive elements
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

# State-wide daily rates
dailyRatesServer <- function(id, covid.confd) {
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
          rate.t = cases.t - lag(cases.t)
        ) %>%
        mutate(
          avg = rollmean(rate.t, 7, fill = NA)
        )
      
      # Date selector input
      output$date_ui <- renderUI({
        ns <- session$ns
        
        dateRangeInput(
          ns("date_rng"),
          "Select Range",
          format = "m/dd/yyyy",
          start  = covid.confd.min,
          end    = covid.confd.max,
          min    = covid.confd.min,
          max    = covid.confd.max
        )
      })
      
      # Target data with default oldest-newest
      rates_data <- reactive({
        req(input$date_rng)
        
        subset(
          covid.confd,
          date >= input$date_rng[1] & date <= input$date_rng[2]
        )
      })
      
      # Main plot
      output$rates <- renderPlotly({
        plot_ly(
          rates_data(),
          hovertemplate = "%{y:,.0f}",
          x             = ~date
        ) %>% add_trace (
          # Average line
          type    = 'scatter',
          mode    = 'lines',
          line    = list(
            color = "black"
          ),
          y       = ~avg,
          name    = "Total"
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
            bordercolor = "black"
          ),
          barmode       = 'group',
          hovermode     = 'x unified',
          hoverdistance = 1,
          spikedistance = 1000
        ) %>% config (
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        )
      })
    }
  )
}

# Display county with the highest XYZ
countyHighestServer <- function(id, covid.local, covid.confd) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare data
      pop <- covid.local %>%
        slice_max(date) %>%
        select(pop) %>%
        sum()
      
      covid.local <- covid.local %>%
        group_by(local) %>%
        arrange(local, date) %>%
        # Systematically rename
        transmute(
          date        = date,
          local       = local,
          total.c     = cases,
          total.h     = hospts,
          total.d     = deaths,
          total.c.adj = cases.adj,
          total.h.adj = hospts.adj,
          total.d.adj = deaths.adj
        ) %>%
        # Calculate daily change version
        mutate(
          rate.c     = total.c     - lag(total.c),
          rate.h     = total.h     - lag(total.h),
          rate.d     = total.d     - lag(total.d),
          rate.c.adj = total.c.adj - lag(total.c.adj),
          rate.h.adj = total.h.adj - lag(total.h.adj),
          rate.d.adj = total.d.adj - lag(total.d.adj)
        ) %>%
        ungroup()
      
      covid.confd <- covid.confd %>%
        arrange(date) %>%
        # Systematically rename
        transmute(
          date        = date,
          total.c     = cases.t,
          total.h     = hospts.t,
          total.d     = deaths.t,
          total.c.adj = cases.t  * 100000 / pop,
          total.h.adj = hospts.t * 100000 / pop,
          total.d.adj = deaths.t * 100000 / pop
        ) %>%
        # Calculate daily change version
        mutate(
          rate.c     = total.c     - lag(total.c),
          rate.h     = total.h     - lag(total.h),
          rate.d     = total.d     - lag(total.d),
          rate.c.adj = total.c.adj - lag(total.c.adj),
          rate.h.adj = total.h.adj - lag(total.h.adj),
          rate.d.adj = total.d.adj - lag(total.d.adj)
        ) 
      
      target <- reactive({
        paste0(
          input$rank, input$mode, if(input$adjust) {".adj"}
        )
      })
      
      # Select target metric
      value <- reactive({
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
        value() %>%
          slice_max(date) %>%
          arrange(desc(target.co))
      })
      
      # County selection, ranked in descending order
      output$list <- renderUI({
        ns <- session$ns
        
        selectInput(
          ns("list"),
          "Select County",
          rank()$local,
          multiple = FALSE,
          selectize = TRUE
        )
      })
      
      # Selected county's data
      selection <- reactive({
        req(input$list)
        
        if(input$rank == "total") {
          value() %>%
            filter(local == input$list) %>%
            left_join(value.va(), by = "date")
        } else {
          value() %>%
            filter(local == input$list) %>%
            left_join(value.va(), by = "date") %>%
            mutate(
              avg.co = rollmean(target.co, k = 7, fill = NA),
              avg.va = rollmean(target.va, k = 7, fill = NA)
            ) %>%
            select(
              date,
              local,
              target.co = avg.co,
              target.va = avg.va
            )
        }
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

      output$chart <- renderPlotly({
        req(input$list)
        
        plot_ly(
          selection(),
          type  = 'scatter',
          mode  = 'lines',
          x     = ~date,
          # County cases
          y     = ~target.co,
          name  = input$list,
          color = I("red"),
          hovertemplate = "%{y:,.0f}"
        ) %>% add_trace(
          # VA average
          y     = ~target.va,
          name  = title_va(),
          color = I("black")
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
            bordercolor = "black"
          ),
          hovermode     = 'x unified',
          hoverdistance = 1,
          spikedistance = 1000
        ) %>% config (
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        )
      })
    }
  )
}

dashboardServer <- function(id, local, covid.confd, pop) {
  moduleServer(
    id,
    function(input, output, session) {
      statsServer("stats", covid.confd, pop)
      mapServer("map", local)
      dailyRatesServer("rates", covid.confd)
      countyHighestServer("highest", local@data, covid.confd)
    }
  )
} 
