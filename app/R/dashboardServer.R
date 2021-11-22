# DASHBOARD SERVER MODULES

# Statistics
statsServer <- function(id, covid.confd) {
  moduleServer(
    id,
    function(input, output, session) {
      # Helper function, changes 10000 to 10,000
      fancy_num <- function(x) {
        format(
          x,
          big.mark   = ",",
          scientific = FALSE,
          trim       = TRUE
        )
      }
      
      # Prepare data
      stats <- covid.confd %>%
        slice_max(date) %>%
        select(cases.c:deaths.t) %>%
        map(fancy_num)
      
      output$cases.t  <- renderText(stats$cases.t)
      output$cases.c  <- renderText(stats$cases.c)
      output$cases.p  <- renderText(stats$cases.p)
      output$hospts.t <- renderText(stats$hospts.t)
      output$hospts.c <- renderText(stats$hospts.c)
      output$hospts.p <- renderText(stats$hospts.p)
      output$deaths.t <- renderText(stats$deaths.t)
      output$deaths.c <- renderText(stats$deaths.c)
      output$deaths.p <- renderText(stats$deaths.p)
    }
  )
}

# Choropleth map
mapServer <- function(id, local) {
  moduleServer(
    id,
    function(input, output, session) {
      # Widget Servers
      type    <- hotspotInputServer("type")
      date_in <- dateInputServer("date", min_date, max_date)
      target  <- targetInputServer("target")
      
      # Date Range
      min_date <- reactive({
        if(type() == "rate") {
          min(local$date) + 7 # 7-Day average kicks in
        } else {
          min(local$date)
        }
      })
      
      max_date <- reactiveVal({
        max(local$date)
      })
      
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
      
      # Draw initial map (only sets up map zoom level)
      output$map <- renderLeaflet({
        bounds <- bbox(local) %>%
          as.vector()
        
        map <- leaflet() %>%
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
          addProviderTiles(providers$CartoDB.PositronNoLabels)
        
        return(map)
      })
      
      # Date selection
      date_sel <- reactive({
        req(date_in())
        
        local %>%
          filter(date == date_in())
      })
      
      # Generate Legend Title
      title <- reactive({
        paste(
          switch(
            type(),
            total = "Total",
            rate  = "Daily"
          ),
          switch(
            target$mode(),
            `c`  = "Cases",
            `h`  = "Hospitalizations",
            `d`  = "Deaths",
            `1d` = "Vaccinations (1+ Doses)",
            `fv` = "Vaccinations (Full)"
          ),
          if(target$adjust()) {
            "per 100k"
          }
        )
      })
      
      # Display value
      value <- reactive({
        date_sel() %>%
          select(
            ends_with(paste0(type(), ".", target$mode(), if(target$adjust()) {".adj"}))
          ) %>%
          # Column 1 -> vector
          .[[1]]
      })
      
      # Color range
      color <- reactive({
        switch(
          target$mode(),
          `c`  = "YlOrRd",
          `h`  = "Purples",
          `d`  = "Greys",
          `1d` = "Greens",
          `fv` = "Greens"
        )
      })
      
      # Binning categories
      categories <- reactive({
        # Use Jenks Natural Breaks to find *8* bin categories
        if(type() == "total") {
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
        if(type() == "total") {
          sprintf(
            "<b>%s</b> (%s)</br>
            Total Cases: %s (%s / 100k)</br>
            Total Hospitalizations: %s (%s / 100k)</br>
            Total Deaths: %s (%s / 100k)</br>
            Total Paritally Vaccinated: %s (%s / 100k)</br>
            Total Fully Vaccinations: %s (%s / 100k)</br>
            Population: %s",
            date_sel()$local, format(date_sel()$date, "%D"),
            fancy_num(date_sel()$total.c),  fancy_num(date_sel()$total.c.adj),
            fancy_num(date_sel()$total.h),  fancy_num(date_sel()$total.h.adj),
            fancy_num(date_sel()$total.d),  fancy_num(date_sel()$total.d.adj),
            fancy_num(date_sel()$total.1d), fancy_num(date_sel()$total.1d.adj),
            fancy_num(date_sel()$total.fv), fancy_num(date_sel()$total.fv.adj),
            fancy_num(date_sel()$pop)
          ) %>%
            lapply(HTML)
        } else {
          sprintf(
            "<b>%s</b> (%s)</br>
            Daily Cases: %s (%s / 100k)</br>
            Daily Hospitalizations: %s (%s / 100k)</br>
            Daily Deaths: %s (%s / 100k)</br>
            Daily Vaccinations (1st Dose): %s (%s / 100k)</br>
            Daily Vaccinations (Final Dose): %s (%s / 100k)</br>
            Population: %s",
            date_sel()$local, format(date_sel()$date, "%D"),
            fancy_num(date_sel()$rate.c),  fancy_num(date_sel()$rate.c.adj),
            fancy_num(date_sel()$rate.h),  fancy_num(date_sel()$rate.h.adj),
            fancy_num(date_sel()$rate.d),  fancy_num(date_sel()$rate.d.adj),
            fancy_num(date_sel()$rate.1d), fancy_num(date_sel()$rate.1d.adj),
            fancy_num(date_sel()$rate.fv), fancy_num(date_sel()$rate.fv.adj),
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
      observeEvent(session$getCurrentTheme(), {
        ns <- session$ns
        
        if(bs_get_variables(session$getCurrentTheme(), "bg") == "#161616") {
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

# State-wide Infection rates
dailyInfectionsServer <- function(id, covid.confd) {
  moduleServer(
    id,
    function(input, output, session) {
      # Suppress warning because first day doesn't have a rate
      covid.confd <- covid.confd %>%
        filter(date >= min(date) + 1)
      
      output$rates <- renderPlotly({
        plot_ly(
          covid.confd,
          hovertemplate = "%{y:,.0f}",
          x             = ~date
        ) %>% add_lines (
          # Average line
          line    = list(
            color = getCurrentOutputInfo()$fg(),
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
          title  = list(
            text = "Daily Virginia COVID-19 Infection Rates",
            font = list(size = 20)
          ),
          legend = list(
            x           = 0.01,
            y           = 1,
            bgcolor     = "transparent",
            bordercolor = "transparent"
          ),
          xaxis = list(
            title          = "Date",
            showgrid       = TRUE,
            gridcolor      = getCurrentOutputInfo()$bg(),
            gridwidth      = 2,
            fixedrange     = TRUE,
            showspikes     = TRUE,
            spikethickness = 2,
            spikedash      = 'dot',
            spikecolor     = "darkgrey",
            spikemode      = 'across'
          ),
          yaxis = list(
            title      = "Cases",
            fixedrange = TRUE,
            showgrid   = TRUE,
            gridcolor  = getCurrentOutputInfo()$bg(),
            gridwidth  = 2,
            dtick      = 2000
          ),
          hoverlabel = list(
            bordercolor = getCurrentOutputInfo()$fg(),
            bgcolor     = getCurrentOutputInfo()$bg()
          ),
          margin = list(t = 50),
          barmode       = 'group',
          hovermode     = 'x unified',
          hoverdistance = 1,
          spikedistance = 1000,
          paper_bgcolor = "transparent",
          plot_bgcolor  = thematic_get_mixture(0.05),
          font = list(
            color = getCurrentOutputInfo()$fg(),
            size  = 14
          )
        ) %>% config (
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        ) %>%
          rangeslider()
      }) %>%
        bindCache(getCurrentOutputInfo()$fg())
    }
  )
}

# State-wide Vaccination rates
dailyVaccinesServer <- function(id, covid.local) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare Data
      vaxs <- covid.local %>%
        group_by(date) %>%
        summarize(rate.1d = sum(rate.1d), rate.fv = sum(rate.fv))
      
      output$rates <- renderPlotly({
        plot_ly(
          vaxs,
          hovertemplate = "%{y:,.0f}",
          x             = ~date
        ) %>% add_lines (
          # Daily First Dose
          line    = list(
            color = "limegreen",
            shape = "spline"
          ),
          y       = ~rate.1d,
          name    = "First Dose"
        ) %>% add_lines (
          # Daily Final Dose
          line    = list(
            color = "darkgreen",
            shape = "spline"
          ),
          y       = ~rate.fv,
          name    = "Final Dose"
        )%>% layout (
          title  = list(
            text = "Daily Virginia COVID-19 Vaccination Rates",
            font = list(size = 20)
          ),
          legend = list(
            x           = 0.01,
            y           = 1,
            bgcolor     = "transparent",
            bordercolor = "transparent"
          ),
          xaxis = list(
            title          = "Date",
            showgrid       = TRUE,
            gridcolor      = getCurrentOutputInfo()$bg(),
            gridwidth      = 2,
            fixedrange     = TRUE,
            showspikes     = TRUE,
            spikethickness = 2,
            spikedash      = 'dot',
            spikecolor     = "darkgrey",
            spikemode      = 'across'
          ),
          yaxis = list(
            title      = "Cases",
            fixedrange = TRUE,
            showgrid   = TRUE,
            gridcolor  = getCurrentOutputInfo()$bg(),
            gridwidth  = 2,
            dtick      = 20000
          ),
          hoverlabel = list(
            bordercolor = getCurrentOutputInfo()$fg(),
            bgcolor     = getCurrentOutputInfo()$bg()
          ),
          margin = list(t = 50),
          barmode       = 'group',
          hovermode     = 'x unified',
          hoverdistance = 1,
          spikedistance = 1000,
          paper_bgcolor = "transparent",
          plot_bgcolor  = thematic_get_mixture(0.05),
          font = list(
            color = getCurrentOutputInfo()$fg(),
            size  = 14
          )
        ) %>% config (
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        ) %>%
          rangeslider()
      })
    }
  )
}

# State-wide Vaccination pie
vaccinationsPieServer <- function(id, covid.local) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare data
      vaxs <- covid.local %>%
        slice_max(date) %>%
        transmute(
          local,
          pop      = pop - total.1d,
          total.1d = total.1d - total.fv,
          total.fv
        ) %>%
        arrange(local) %>%
        add_row(
          local = "Virginia",
          pop = sum(.$pop),
          total.1d = sum(.$total.1d),
          total.fv = sum(.$total.fv),
          .before = 1
        ) %>%
        rename(
          `Unvaccinated`     = pop,
          `One Dose`         = total.1d,
          `Fully Vaccinated` = total.fv
        ) %>%
        pivot_longer(!local, names_to = "label", values_to = "val")
      
      # Widget Server
      county <- countyInputServer("county", reactiveVal(unique(vaxs$local)))
      
      # County Selection
      county_data <- reactive({
        req(county)
        
        vaxs %>%
          filter(local == county())
      })
      
      output$pie <- renderPlotly({
        plot_ly(
          county_data(),
          labels = ~label,
          values = ~val,
          type   = "pie",
          textposition   = 'inside',
          textinfo       = 'label+percent',
          insidetextfont = list(
            color = getCurrentOutputInfo()$fg()
          ),
          hovertemplate = "%{y:,.0f}",
          marker        = list(
            colors  = c('grey', 'limegreen', 'darkgreen'),
            line    = list(
              color = 'white'
            )
          ),
          showlegend = FALSE
        ) %>% layout(
          title = paste("Vaccination in", county()),
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          font = list(
            color = getCurrentOutputInfo()$fg()
          ),
          hoverlabel = list(
            bordercolor = getCurrentOutputInfo()$fg(),
            bgcolor     = getCurrentOutputInfo()$bg()
          )
        )
      })
    }
  )
}

# Display county with the highest XYZ
countyHighestServer <- function(id, covid.local) {
  moduleServer(
    id,
    function(input, output, session) {
      # Widget Servers
      type   <- hotspotInputServer("rank")
      mode   <- targetInputServer("mode")
      county <- countyInputServer("county", reactive(rank_list()$local))
      
      # State Average Data
      covid.va <- covid.local %>%
        group_by(date) %>%
        summarise(fips = 51000, local = "Virginia", across(total.c:pop, ~sum(.x))) %>%
        mutate(
          across(total.c:pop, ~.x * 100000 / pop, .names = "{.col}.adj")
        )
      
      # Calculate target column
      target <- reactive({
        paste0(
          type(), ".", mode$mode(), if(mode$adjust()) {".adj"}
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
        covid.va %>%
          select(
            date,
            target.va = ends_with(target())
          )
      })
      
      # Rank values for selection input
      rank_list <- reactive({
        value.co() %>%
          slice_max(date) %>%
          arrange(desc(target.co))
      })
      
      # Selected county's data
      selection <- reactive({
        req(county() != "Placeholder")
        
        value.co() %>%
          filter(local == county()) %>%
          inner_join(value.va(), by = "date")
      })
      
      title_plot <- reactive({
        req(county() != "Placeholder")
        
        paste(
          switch(
            type(),
            "total" = "Total",
            "rate"  = "Daily"
          ),
          switch(
            mode$mode(),
            "c"  = "Cases",
            "h"  = "Hospitalizations",
            "d"  = "Deaths",
            "1d" = "Vaccinations (1+ Doses)",
            "fv" = "Vaccinations (Full)"
          ),
          "in",
          county(),
          "vs. State",
          if(mode$adjust()) {
            "Average (Population Adjusted)"
          } else {
            "Total"
          }
        )
      })
      
      title_yaxis <- reactive({
        paste(
          switch(
            mode$mode(),
            "c"  = "Cases",
            "h"  = "Hospitalizations",
            "d"  = "Deaths",
            "1d" = "Vaccinations (1+ Doses)",
            "fv" = "Vaccinations (Full)"
          ),
          if(mode$adjust()) {
            "per 100k"
          }
        )
      })
      
      title_va <- reactive({
        paste(
          "VA",
          if(mode$adjust()) {
            "Average"
          } else {
            "Total"
          }
        )
      })
      
      line_col <- reactive({
        switch(
          mode$mode(),
          "c"  = "red",
          "h"  = "blue",
          "d"  = "gray",
          "1d" = "limegreen",
          "fv" = "darkgreen"
        )
      })
      
      output$chart <- renderPlotly({
        req(county() != "Placeholder")

        plot_ly(
          selection(),
          x     = ~date,
          hovertemplate = "%{y:,.0f}"
        ) %>% add_lines (
          # County cases
          y    = ~target.co,
          name = county(),
          line = list(
            color = line_col(),
            shape = "spline"
          )
        ) %>% add_lines (
          # VA average
          y    = ~target.va,
          name = title_va(),
          line = list(
            color = getCurrentOutputInfo()$fg(),
            shape = "spline"
          )
        ) %>% layout (
          # Setup
          title = title_plot(),
          legend = list(
            x           = 0.01,
            y           = 1,
            bgcolor     = "transparent",
            bordercolor = "transparent"
          ),
          yaxis = list(
            title      = list(
              text = title_yaxis(),
              font = list(size = 20)
            ),
            showgrid   = TRUE,
            gridcolor  = getCurrentOutputInfo()$bg(),
            gridwidth  = 2,
            fixedrange = TRUE
          ),
          xaxis = list(
            title          = "Date",
            showgrid       = TRUE,
            gridcolor      = getCurrentOutputInfo()$bg(),
            gridwidth      = 2,
            fixedrange     = TRUE,
            showspikes     = TRUE,
            spikethickness = 2,
            spikedash      = 'dot',
            spikecolor     = "darkgrey",
            spikemode      = 'across'
          ),
          hoverlabel = list(
            bordercolor = getCurrentOutputInfo()$fg(),
            bgcolor     = getCurrentOutputInfo()$bg()
          ),
          margin = list(t = 50),
          hovermode     = 'x unified',
          hoverdistance = 1,
          spikedistance = 1000,
          paper_bgcolor = "transparent",
          plot_bgcolor  = thematic_get_mixture(0.05),
          font = list(
            color = getCurrentOutputInfo()$fg(),
            size  = 14
          )
        ) %>% config (
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        ) %>%
          rangeslider()
      }) %>% bindCache(
        getCurrentOutputInfo()$fg(),
        target(),
        county()
      )
    }
  )
}

dashboardServer <- function(id, local, covid.confd) {
  moduleServer(
    id,
    function(input, output, session) {
      statsServer("stats", covid.confd)
      mapServer("map", local)
      dailyInfectionsServer("cases", covid.confd)
      dailyVaccinesServer("vaxs", local@data)
      vaccinationsPieServer("pie", local@data)
      countyHighestServer("highest", local@data)
    }
  )
} 
