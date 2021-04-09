## LIBRARIES ###################################################################
library(shiny)     # Building interactive websites
library(rgdal)     # Reading spatial data files
library(RSocrata)  # Reading data from socrata sites (VDH)
library(sp)        # Working with spatial data
library(leaflet)   # Graphing interactive maps
library(dplyr)     # Arranging & modifying data
library(zoo)       # Calculating a rolling mean
library(plotly)    # Graphing interactive plots
library(BAMMtools) # Jenks breaks
## STATIC DATA #################################################################

## Retrieve Data

cases <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json")
confd <- read.socrata("https://data.virginia.gov/resource/uqs3-x7zh.json")
pop   <- read.socrata("https://data.virginia.gov/resource/5s4f-hthh.json")
spdf  <- readOGR(
    dsn = "./DATA/shapefile",
    layer = "cb_2019_us_county_500k"
)

##  Fix Data

# Changing types
cases$report_date       <- as.Date(cases$report_date)
cases$total_cases       <- as.numeric(cases$total_cases)
cases$deaths            <- as.numeric(cases$deaths)
cases$hospitalizations  <- as.numeric(cases$hospitalizations)

confd$report_date       <- as.Date(confd$report_date)
confd$number_of_cases   <- as.numeric(confd$number_of_cases)

pop$population_estimate <- as.numeric(pop$population_estimate)

# Delete unneeded cols
cases <- cases %>% select(!c(vdh_health_district))

confd <- confd %>% select(!c(number_of_deaths, 
                             number_of_hospitalizations))

pop <- subset(pop, year == max(year))
pop <- pop %>% select(!c(health_district,
                         health_region, year)) %>%
    rename(est = population_estimate)

# use only VA info
spdf <- subset(spdf, STATEFP == "51")
# we just need the fips code, dump other data and rename GEOID to fips
spdf@data <- spdf@data %>% select(fips = GEOID)
# now kiss
spdf <- merge(spdf, cases, duplicateGeoms = TRUE)
rm(cases)

# split confirmed and probable cases
confd.c <- subset(confd, case_status == "Confirmed") %>% arrange(report_date)
confd.p <- subset(confd, case_status == "Probable")  %>% arrange(report_date)
# recombine as columns
confd <- data.frame(report_date = confd.c$report_date,
                    cases.c = confd.c$number_of_cases,
                    cases.p = confd.p$number_of_cases)
# remove temp values
rm(confd.c, confd.p)

# get total current population estimates for map
pop.total <- pop %>%
    group_by(fips) %>%
    summarize(pop = sum(est))
spdf <- merge(spdf, pop.total)
rm(pop.total)

## Custom data

# Min and max dates in data
spdf.min <- min(spdf$report_date)
spdf.max <- max(spdf$report_date)

confd.min <- min(confd$report_date)
confd.max <- max(confd$report_date)

## Generated Data

# Get the covid cases based on population density
spdf@data <- spdf@data %>%
    mutate(total_cases.adj = total_cases * 100000 / pop,
           hospitalizations.adj = hospitalizations * 100000 / pop,
           deaths.adj = deaths * 100000 / pop)

# Calculate the daily rate of confirmed vs probable cases
confd <- confd %>%
    # Sort by report date
    arrange(report_date) %>%
    # Total cases
    mutate(cases.t = cases.c + cases.p) %>%
    # Rates of confirmed, probable, total
    mutate(rate.c = cases.c - lag(cases.c),
           rate.p = cases.p - lag(cases.p),
           rate.t = cases.t - lag(cases.t)) %>%
    # 7-day moving average of total
    mutate(avg = rollmean(rate.t, 7, fill = NA))

# Current totals of cases, etc;
stats.c = sum(spdf$total_cases[spdf$report_date == spdf.max])
stats.h = sum(spdf$hospitalizations[spdf$report_date == spdf.max])
stats.d = sum(spdf$deaths[spdf$report_date == spdf.max])
stats.p = sum(spdf$pop[spdf$report_date == spdf.max])

# Case rates for each county
rates <- spdf@data %>%
    select(report_date = report_date, name = locality,
           id = fips, cases = total_cases.adj) %>%
    group_by(id) %>%
    arrange(id, report_date) %>%
    mutate(rate = cases - lag(cases)) %>%
    mutate(rate.avg = rollmean(rate, 7, fill = NA)) %>%
    ungroup()

rates.va <- confd %>%
    transmute(va_cases    = cases.t * 100000 / stats.p,
              va_rate     = rate.t  * 100000 / stats.p,
              va_rate.avg = avg     * 100000 / stats.p,
              report_date = report_date)

## SERVER FUNCTIONS ############################################################
shinyServer(function(input, output, session) {
    
    ## DASHBOARD PAGE ##########################################################
    
    # Quick Statistics
    output$db_stats <- renderUI({
        paste0("Total Cases: ", stats.c, "</br>",
               "Total Hospitalizations: ", stats.h, "</br>",
               "Total Deaths: ", stats.d, "</br>") %>%
            lapply(htmltools::HTML)
    })
    
    ## Map Section
    
    # Date selector
    output$db_date_ui <- renderUI({
        dateInput("db_date", "Select Date:",
                  min = spdf.min, 
                  max = spdf.max,
                  value = spdf.max)
    })
    
    # Date Selection
    db_date_sel <- reactive({
        # Prevent errors while loading ui
        if(length(input$db_date) == 0) {
            subset(spdf, report_date == spdf.max)
        }
        else {
            subset(spdf, report_date == input$db_date)
        }
    })
    
    # Change legend title for selection
    db_target_title <- reactive({
        if(input$db_pop_adj) {
            switch (input$db_mode,
                    pop="Total Population",
                    cases="Cases per 100k",
                    hosp="Hospitalizations per 100k",
                    deaths="Deaths per 100k"
            )
        } else {
            switch (input$db_mode,
                    pop="Total Population",
                    cases="Total Cases",
                    hosp="Total Hospitalizations",
                    deaths="Total Deaths"
            )
        }
    })
    
    # Change choropleth target
    db_target_val <- reactive({
        if(input$db_pop_adj) {
            switch (input$db_mode,
                    pop=db_date_sel()$pop,
                    cases=db_date_sel()$total_cases.adj,
                    hosp=db_date_sel()$hospitalizations.adj,
                    deaths=db_date_sel()$deaths.adj
            )
        } else {
            switch (input$db_mode,
                    pop=db_date_sel()$pop,
                    cases=db_date_sel()$total_cases,
                    hosp=db_date_sel()$hospitalizations,
                    deaths=db_date_sel()$deaths
            )
        }
    })
    
    # Change choropleth color scheme
    db_target_col <- reactive({
        switch (input$db_mode,
                pop="Greens",
                cases="YlOrRd",
                hosp="Purples",
                deaths="Greys"
        )
    })
    
    # Categories for map
    db_map_categories <- reactive({
        # Use Jenks Natural Breaks to find *8* bin categories
        categories <- getJenksBreaks(db_target_val(), 9)
        
        # Beautify breaks
        categories <- unique(ceiling(signif(categories, digits = 3)))
        categories[length(categories)] <- Inf
        categories[1] <- 0
        
        return(categories)
    })
    
    # Function to color map
    db_map_cols <- reactive({
        colorBin(palette = db_target_col(),
                 domain = db_target_val(),
                 bins = db_map_categories(),
                 na.color = "black")
    })
    
    # Tooltips for map
    db_map_tooltip <- reactive({
        paste0(
            "<b>", db_date_sel()$locality, "</b></br>",
            "Total Cases: ", db_date_sel()$total_cases, "</br>",
            "Hospitalizations: ", db_date_sel()$hospitalizations, "</br>",
            "Deaths: ", db_date_sel()$deaths, "</br>",
            "Population: ", db_date_sel()$pop, "</br></br>",
            "Cases per 100k: ", 
            floor(db_date_sel()$total_cases.adj), "</br>",
            "Hospitalizations per 100k: ", 
            floor(db_date_sel()$hospitalizations.adj), "</br>",
            "Deaths per 100k: ", 
            floor(db_date_sel()$deaths.adj), "</br>"
        ) %>% 
        lapply(htmltools::HTML)
    })
    
    # Static parts of map
    output$db_map <- renderLeaflet({
        # Set default view to bounding box of VA
        bounds <- bbox(spdf) %>% as.vector()
        leaflet(spdf) %>%
            fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    })
    
    # Update map polygons
    observe({
        pal <- db_map_cols()
        
        leafletProxy("db_map", data = db_date_sel()) %>%
            clearShapes() %>%
            addPolygons(
                # Set color
                fillColor = ~pal(db_target_val()),
                fillOpacity = 1.0,
                
                # Add Borders
                stroke = T,
                color = "grey",
                weight = 0.6,
                
                # Add tooltip
                label = db_map_tooltip(),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                )
            )
    })
    
    # Update map legend
    observe({
        pal <- db_map_cols()
        
        leafletProxy("db_map", data = db_date_sel()) %>%
            clearControls() %>%
            addLegend(position = "topleft", pal = pal, opacity = 0.9,
                      title = db_target_title(), values = db_target_val())
    })
    
    ## Daily Rates Section
    
    # Date range input
    output$db_date_rng_ui <- renderUI({
        dateRangeInput("db_date_rng", "Select Range",
                       start = confd.min,
                       end   = confd.max,
                       min   = confd.min,
                       max   = confd.max)
    })
    
    # Selected rates (Confirmed Vs Unconfirmed)
    db_rates_data <- reactive({
        # Get range
        if(length(input$db_date_rng == 2)) {
            subset(confd, report_date >= input$db_date_rng[1] &
                       report_date <= input$db_date_rng[2])
        } else {
            subset(confd)
        }
    })
    
    # Create daily rates diagram
    output$db_rates <- renderPlotly({
        # Tooltip for confirmed
        text_conf <- paste0(
            "Date: ", db_rates_data()$report_date, "\n",
            "Confirmed Cases: ", db_rates_data()$rate.c, "\n"
        )
        # Tooltip for probable
        text_prob <- paste0(
            "Date: ", db_rates_data()$report_date, "\n",
            "Probable Cases: ", db_rates_data()$rate.p, "\n"
        )
        # Tooltip for average line
        text_avg <- paste0(
            "Total Cases: ", db_rates_data()$rate.t, "\n",
            "7-day Moving Average: ", floor(db_rates_data()$avg), "\n"
        )
        # Generate plot
        plot_ly(
            # Confirmed rates
            db_rates_data(),
            x = ~report_date, type = 'bar',
            hoverinfo = 'text',
            y = ~rate.c, name = "Confirmed",
            text = text_conf, color = I("darkred")
        ) %>% add_trace(
            # Unconfirmed rates
            y = ~rate.p, name = "Probable",
            text = text_prob, color = I("red")
        ) %>% add_trace(
            # Average lines
            y = ~avg, name = "Total (7-Day Average)",
            text = text_avg, line = list(
                color = "black"
            ),
            type = 'scatter', mode = 'lines'
        ) %>% layout (
            # Labels & Setup
            title = "Daily Virginia COVID-19 Rates",
            legend = list(x = 0, y = 1,
                          bgcolor = "transparent",
                          bordercolor = "transparent"),
            yaxis = list(title = "Cases", showgrid = FALSE, fixedrange = TRUE),
            xaxis = list(title = "", showgrid = FALSE, fixedrange = TRUE,
                         # Add spike line
                         showspikes = TRUE,
                         spikethickness = 2,
                         spikedash = 'dot',
                         spikecolor = "darkgrey",
                         spikemode = 'across'),
            hoverlabel = list(bordercolor = "black"),
            barmode = 'group',
            hovermode = 'x unified',
            hoverdistance = 1,
            spikedistance = 1000
        ) %>% config(
            displayModeBar = FALSE,
            displaylogo = FALSE,
            showTips = FALSE
        )
    })
    
    ## County with highest cases
    
    output$db_highest_cases <- renderPlotly({
        # Get county with highest current cases
        highest_co <- rates %>%
            slice_max(report_date) %>%
            slice_max(cases, with_ties = F)
        
        # Now get that whole county's data
        highest_co <- subset(rates, id == highest_co$id)
        # And add VA averages
        highest_co <- merge(highest_co, rates.va)
        
        # Generate tooltips
        text_county <- paste0(
            "County: ", highest_co$name, "\n",
            "Date: ", highest_co$report_date, "\n",
            "Cases (per 100k): ", floor(highest_co$cases), "\n"
        )
        text_va <- paste0(
            "VA Average\n",
            "Date: ", highest_co$report_date, "\n",
            "Cases (per 100k): ", floor(highest_co$va_cases), "\n"
        )
        
        # Generate plot
        plot_ly(
            highest_co,
            x = ~report_date, type = 'scatter', mode = 'lines',
            hoverinfo = 'text',
            # County cases
            y = ~cases, name = highest_co$name,
            text = text_county, color = I("red")
        ) %>% add_trace(
            # VA Cases
            y = ~va_cases, name = "VA Average",
            text = text_va, color = I("black")
        ) %>% layout (
            # Labels & Setup
            title = paste0("Cases in ", highest_co$name, " vs. State Average",
                           "\n(Population Adjusted)"),
            legend = list(x = 0, y = 1,
                          bgcolor = "transparent",
                          bordercolor = "transparent"),
            yaxis = list(title = "Cases (Per 100k)", showgrid = FALSE,
                         fixedrange = TRUE),
            xaxis = list(title = "", showgrid = FALSE, fixedRange = TRUE,
                         # Add spike line
                         showspikes = TRUE,
                         spikethickness = 2,
                         spikedash = 'dot',
                         spikecolor = "darkgrey",
                         spikemode = 'across'),
            hovermode = 'x',
            hoverdistance = 100,
            spikedistance = 1000
        ) %>% config(
            displayModeBar = FALSE,
            displaylogo = FALSE,
            showTips = FALSE
        )
    })
    
    ## County with the highest daily rate
    
    output$db_highest_rates <- renderPlotly({
        # Get county with highest daily rate
        highest_co <- rates %>%
            slice_max(report_date) %>%
            slice_max(rate, with_ties = F)
        
        # Now get that whole county's data
        highest_co <- subset(rates, id == highest_co$id)
        # And add VA averages
        highest_co <- merge(highest_co, rates.va)
        
        # Generate tooltips
        text_county <- paste0(
            "County: ", highest_co$name, "\n",
            "Date: ", highest_co$report_date, "\n",
            "Daily Cases (per 100k): ", floor(highest_co$rate), "\n",
            "7-Day Average (per 100k): ", floor(highest_co$rate.avg), "\n"
        )
        text_va <- paste0(
            "VA Average\n",
            "Date: ", highest_co$report_date, "\n",
            "Daily Cases (per 100k): ", floor(highest_co$va_rate), "\n",
            "7-Day Average (per 100k): ", floor(highest_co$va_rate.avg), "\n"
        )
        
        # Generate plot
        plot_ly(
            highest_co,
            x = ~report_date, type = 'scatter', mode = 'lines',
            hoverinfo = 'text',
            # County Rate
            y = ~rate.avg, name = paste0(highest_co$name, " (7-Day Average)"),
            text = text_county, color = I("red")
        ) %>% add_trace(
            # VA Rate
            y = ~va_rate.avg, name = "VA Average (7-Day Average)",
            text = text_va, color = I("black")
        ) %>% layout (
            # Labels & Setup
            title = paste0("Daily Rates in ", highest_co$name, 
                           " vs. State Average\n(Population Adjusted)"),
            legend = list(x = 0, y = 1,
                          bgcolor = "transparent",
                          bordercolor = "transparent"),
            yaxis = list(title = "Daily Cases (Per 100k)", showgrid = FALSE,
                         fixedrange = TRUE),
            xaxis = list(title = "", showgrid = FALSE, fixedrange = TRUE,
                         # Add spike line
                         showspikes = TRUE,
                         spikethickness = 2,
                         spikedash = 'dot',
                         spikecolor = "darkgrey",
                         spikemode = 'across'),
            barmode = 'group',
            hovermode = 'x',
            hoverdistance = 100,
            spikedistance = 1000
        ) %>% config(
            displayModeBar = FALSE,
            displaylogo = FALSE,
            showTips = FALSE
        )
    })
    
    ## BY COUNTRY PAGE #########################################################
    ## DEMOGRAPHICS PAGE #######################################################
})
