## LIBRARIES ###################################################################
library(shiny)
library(rgdal)
library(RSocrata)
library(sp)
library(leaflet)
library(dplyr)
library(zoo)
library(plotly)
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

# Color bins for main choropleth
categories.total <- c(0, 1.5625, 3.125, 6.25, 12.5, 25, 50, 75, Inf) / 100
categories.rate  <- c(0, 25, 50, 75, Inf) / 100
# Min and max dates in data
spdf.min <- min(spdf$report_date)
spdf.max <- max(spdf$report_date)

confd.min <- min(confd$report_date)
confd.max <- max(confd$report_date)

## Generated Data

# Get the covid cases based on population density
spdf@data <- spdf@data %>%
    mutate(cases_pop = total_cases * 100000 / pop)

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

## SERVER FUNCTIONS ############################################################
shinyServer(function(input, output) {
    
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
    
    # Main choropleth map
    output$db_map <- renderLeaflet({
        # Convert bins from percentages to numbers
        categories <- categories.total * max(db_date_sel()$total_cases)
        # Round and use only unique values
        categories <- unique(ceiling(signif(categories, digits = 3)))
        
        # Create tooltip
        tooltip <- paste0(
            "<b>", db_date_sel()$locality, "</b></br>",
            "Covid Cases: ", db_date_sel()$total_cases, "</br>",
            "Hospitalizations: ", db_date_sel()$hospitalizations, "</br>",
            "Deaths: ", db_date_sel()$deaths, "</br>") %>% 
            lapply(htmltools::HTML)
        
        # Apply color shading to bins
        colorF <- colorBin(palette = "YlOrRd", 
                           domain = db_date_sel()$total_cases,
                           na.color = "black",
                           bins = categories)
        
        # Create Actual map
        leaflet(db_date_sel()) %>%
        # Polygon data
        addPolygons(
            # Get color
            fillColor = ~colorF(db_date_sel()$total_cases),
            # Make tiles opaque
            fillOpacity = 1.0,
            # Add thin gray border
            stroke = T, color = "grey", weight = 0.6,
            # Add stylized tooltip
            label = tooltip,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
            )) %>%
        # And lastly add a legend
        addLegend( pal=colorF, values=db_date_sel()$total_cases, opacity = 0.9,
                   title = "Total Covid Cases", position = "topleft" )
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
            "Cases: ", db_rates_data()$rate.c, "\n",
            "Status: Confirmed\n"
        )
        # Tooltip for probable
        text_prob <- paste0(
            "Date: ", db_rates_data()$report_date, "\n",
            "Cases: ", db_rates_data()$rate.p, "\n",
            "Status: Probable\n"
        )
        # Tooltip for average line
        text_avg <- paste0(
            "Report Date: ", db_rates_data()$report_date, "\n",
            "Total Cases: ", db_rates_data()$rate.t, "\n",
            "7-day Moving Average: ", db_rates_data()$avg, "\n"
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
            y = ~avg, name = "7-Day Average",
            text = text_avg, line = list(
                color = 'black'
            ),
            type = 'scatter', mode = 'lines'
        ) %>% layout (
            # Labels & Setup
            yaxis = list(title = "Cases"),
            xaxis = list(title = "Report Date"),
            barmode = 'group'
        )
    })
    
    ## BY COUNTRY PAGE #########################################################
    ## DEMOGRAPHICS PAGE #######################################################
})
