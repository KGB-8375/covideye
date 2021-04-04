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
spdf  <- readOGR(
    dsn = "./DATA/shapefile",
    layer = "cb_2019_us_county_500k"
)

##  Fix Data
cases$report_date      <- as.Date(cases$report_date)
cases$total_cases      <- as.numeric(cases$total_cases)
cases$deaths           <- as.numeric(cases$deaths)
cases$hospitalizations <- as.numeric(cases$hospitalizations)

confd$report_date      <- as.Date(confd$report_date)
confd$number_of_cases  <- as.numeric(confd$number_of_cases)
# Delete unneeded cols
cases <- cases %>% select(!c(vdh_health_district))

confd <- confd %>% select(!c(number_of_deaths, 
                             number_of_hospitalizations))

# use only VA info
spdf <- subset(spdf, STATEFP == "51")
# we just need the fips code, dump other data and rename GEOID to fips
spdf@data <- spdf@data %>% select(fips = GEOID)
# now kiss
spdf <- merge(spdf, cases, duplicateGeoms = TRUE)
rm(cases)

# split confirmed and probable cases
confd.c <- subset(confd, case_status == "Confirmed")
confd.p <- subset(confd, case_status == "Probable")
# recombine as columns
confd <- data.frame(report_date = confd.c$report_date,
                    cases.c = confd.c$number_of_cases,
                    cases.p = confd.p$number_of_cases)
# remove temp values
rm(confd.c, confd.p)

## Custom data
# Color bins for main choropleth
mybins <- c(0, 1.5625, 3.125, 6.25, 12.5, 25, 50, 75, Inf) / 100
# Min and max dates in data
spdf.min <- min(spdf$report_date)
spdf.max <- max(spdf$report_date)

confd.min <- min(confd$report_date)
confd.max <- max(confd$report_date)

## Generated Data
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

## SERVER FUNCTIONS ############################################################
shinyServer(function(input, output) {
    
    ## DASHBOARD PAGE ##########################################################
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
        # Convert bins from percentages to numbers (Rounded)
        bins_f <- unique(ceiling(signif(mybins * max(db_date_sel()$total_cases),
                                 digits = 3)))
        
        # Create tooltip
        tooltip <- paste(
                    "<b>", db_date_sel()$locality, "</b></br>",
                    "Covid Cases: ", db_date_sel()$total_cases, "</br>",
                    "Hospitilizations: ", db_date_sel()$hospitalizations, "</br>",
                    "Deaths: ", db_date_sel()$deaths, "</br>",
                    sep = "") %>% 
                  lapply(htmltools::HTML)
        
        # Apply color shading to bins
        mycols <- colorBin(palette = "YlOrRd", 
                           domain = db_date_sel()$total_cases,
                           na.color = "black",
                           bins = bins_f)
        
        # Create Actual map
        leaflet(db_date_sel()) %>%
        # Crop view to just show VA
        fitBounds( lat1 = 36.585007, lng1 = -83.714340,
                   lat2 = 39.447387, lng2 = -74.869230) %>%
        # Polygon data
        addPolygons(
            # Get color
            fillColor = ~mycols(db_date_sel()$total_cases),
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
        addLegend( pal=mycols, values=db_date_sel()$total_cases, opacity = 0.9,
                   title = "Total Covid Cases", position = "topleft" )
    })
    
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
        #tooltip_bars <- paste0(
        #    "Date: ", db_rates_data()$report_date, "\n",
        #    "Cases: ", db_rates_data()$rate, "\n",
        #    "Status: ", db_rates_data()$case_status, "\n"
        #)
        
        #tooltip_avg <- paste0(
        #    "Report Date: ", db_total_rates_data()$report_date, "\n",
        #    "Total Cases: ", db_total_rates_data()$total_rate, "\n",
        #    "7-day Moving Average: ", db_total_rates_data()$rolling_rate, "\n"
        #)
        
        plot_ly(
            db_rates_data(),
            x = ~report_date, type = "bar",
            y = ~rate.c, name = "Confirmed"
        ) %>% add_trace(
            y = ~rate.p, name = "Probable"
        ) %>% layout (
            yaxis = list(title = "Cases"), barmode = "group"
        )
    })
    ## BY COUNTRY PAGE #########################################################
    ## DEMOGRAPHICS PAGE #######################################################
})
