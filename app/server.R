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
age <- read.socrata("https://data.virginia.gov/resource/uktn-mwig.json")
race <- read.socrata("https://data.virginia.gov/resource/9sba-m86n.json")
sex <- read.socrata("https://data.virginia.gov/resource/tdt3-q47w.json")
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
confd$number_of_deaths  <- as.numeric(confd$number_of_deaths)
confd$number_of_hospitalizations <-
    as.numeric(confd$number_of_hospitalizations)

pop$population_estimate <- as.numeric(pop$population_estimate)

age$report_date <- as.Date(age$report_date)
age <- subset(age, report_date >= as.Date("2020-04-21"))
age$number_of_cases <- as.numeric(age$number_of_cases)
age$number_of_hospitalizations <- 
    as.numeric(age$number_of_hospitalizations)
age$number_of_deaths <- as.numeric(age$number_of_deaths)

race$report_date <- as.Date(race$report_date)
race$number_of_cases <- as.numeric(race$number_of_cases)
race$number_of_hospitalizations <- 
    as.numeric(race$number_of_hospitalizations)
race$number_of_deaths <- as.numeric(race$number_of_deaths)

sex$report_date <- as.Date(sex$report_date)
sex <- subset(sex, report_date >= as.Date("2020-04-13"))
sex$number_of_cases <- as.numeric(sex$number_of_cases)
sex$number_of_hospitalizations <- 
    as.numeric(sex$number_of_hospitalizations)
sex$number_of_deaths <- as.numeric(sex$number_of_deaths)

# Delete unneeded cols
cases <- cases %>% select(!c(vdh_health_district))

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
                    cases.p = confd.p$number_of_cases,
                    deaths.c = confd.c$number_of_deaths,
                    deaths.p = confd.p$number_of_deaths,
                    hosp.c = confd.c$number_of_hospitalizations,
                    hosp.p = confd.p$number_of_hospitalizations)
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

age_max <- max(age$report_date)
age_min <- "2020-04-21"

race_max <- max(race$report_date)
race_min <- min(race$report_date)

sex_max <- max(sex$report_date)
sex_min <- "2020-04-13"

demo_max <- min(sex_max, age_max, race_max)
demo_min <- max(sex_min, age_min, race_min)

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
    mutate(cases.t = cases.c + cases.p,
           deaths.t = deaths.c + deaths.p,
           hosp.t = hosp.c + hosp.p) %>%
    # Rates of confirmed, probable, total
    mutate(rate.c = cases.c - lag(cases.c),
           rate.p = cases.p - lag(cases.p),
           rate.t = cases.t - lag(cases.t)) %>%
    # 7-day moving average of total
    mutate(avg = rollmean(rate.t, 7, fill = NA))

# Statistics numbers
stats <- subset(confd, report_date == confd.max)
stats.p <- sum(pop$est)

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

## Data for Age Category

# Grouping Age Data
age <- age %>%
    group_by(report_date, age_group) %>%
    summarise(cases = sum(number_of_cases),
              hosp = sum(number_of_hospitalizations),
              deaths = sum(number_of_deaths))

# Age Population
pop.age <- pop %>%
    group_by(age_group) %>%
    summarise(est = sum(est))

# Find adjusted age data
age.adj <- merge(age, pop.age, all.x=F, all.y=T)
age.adj <- age.adj %>%
    mutate(cases.adj = cases*100000/est,
           hosp.adj = hosp*100000/est,
           deaths.adj = deaths*100000/est)

## Data for Race Category

# Grouping Race Data
race <- race %>% 
    group_by(report_date, race_and_ethnicity) %>%
    summarise(cases = sum(number_of_cases),
              hosp = sum(number_of_hospitalizations),
              deaths = sum(number_of_deaths))

# Race Population
pop.race <- pop %>%
    group_by(race_and_ethnicity) %>%
    summarise(est = sum(est))

# Find adjusted race data
race.adj <- merge(race, pop.race, all.x=F, all.y=T)
race.adj <- race.adj %>%
    mutate(cases.adj = cases*100000/est,
           hosp.adj = hosp*100000/est,
           deaths.adj = deaths*100000/est)

## Data for Sex Category

# Grouping Sex Data
sex <- sex %>%
    group_by(report_date, sex) %>%
    summarise(cases = sum(number_of_cases),
              hosp = sum(number_of_hospitalizations),
              deaths = sum(number_of_deaths))

# Sex Population
pop.sex <- pop %>%
    group_by(sex) %>%
    summarise(est = sum(est))

# Find adjusted sex data
sex.adj <- merge(sex, pop.sex, all.x=F, all.y=T)
sex.adj <- sex.adj %>%
    mutate(cases.adj = cases*100000/est,
           hosp.adj = hosp*100000/est,
           deaths.adj = deaths*100000/est)

## SERVER FUNCTIONS ############################################################
shinyServer(function(input, output, session) {
    
    ## DASHBOARD PAGE ##########################################################
    
    # Quick Statistics
    output$db_stats <- renderUI({
        wellPanel(
            fluidRow(
                column(4, h2("Total Cases: ", 
                             format(stats$cases.t, 
                                    big.mark = ',', 
                                    scientific = F), 
                             align = 'center'),
                       
                       h3("(", format(stats$cases.c,
                                      big.mark = ',',
                                      scientific = F)
                          , " Confirmed )", 
                          align = 'center')),
                
                column(4, h2("Total Hospitalizations: ", 
                             format(stats$hosp.t,
                                    big.mark = ',',
                                    scientific = F),
                             align = 'center'),
                       
                       h3("(", format(stats$hosp.c,
                                      big.mark = ',',
                                      scientific = F),
                          " Confirmed )",
                          align = 'center')),
                
                column(4, h2("Total Deaths: ", 
                             format(stats$deaths.t,
                                    big.mark = ',',
                                    scientific = F),
                             align = 'center'),
                       
                       h3("(", format(stats$deaths.c,
                                      big.mark = ',',
                                      scientific = F), 
                          " Confirmed )",
                          align = 'center'))
            )
        )
    })
    
    ## Map Section
    
    # Date selector
    output$db_date_ui <- renderUI({
        dateInput("db_date", "Select Date:",
                  min = spdf.min, 
                  max = spdf.max,
                  value = spdf.max,
                  format = "mm/dd/yy")
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
            "<b>", db_date_sel()$locality, "</b> (",
            format(db_date_sel()$report_date, "%D"),")</br>",
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
                    direction = "right"
                )
            )
    })
    
    # Update map legend
    observe({
        pal <- db_map_cols()
        
        leafletProxy("db_map", data = db_date_sel()) %>%
            clearControls() %>%
            addLegend(position = "bottomleft", pal = pal, opacity = 0.9,
                      title = db_target_title(), values = db_target_val())
    })
    
    ## Daily Rates Section
    
    # Date range input
    output$db_date_rng_ui <- renderUI({
        dateRangeInput("db_date_rng", "Select Range",
                       start = confd.min,
                       end   = confd.max,
                       min   = confd.min,
                       max   = confd.max,
                       format = "mm/dd/yy")
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
            "Date: ", format(db_rates_data()$report_date, "%D"), "\n",
            "Confirmed Cases: ", db_rates_data()$rate.c, "\n"
        )
        # Tooltip for probable
        text_prob <- paste0(
            "Date: ", format(db_rates_data()$report_date, "%D"), "\n",
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
            "Date: ", format(highest_co$report_date, "%D"), "\n",
            "Cases (per 100k): ", floor(highest_co$cases), "\n"
        )
        text_va <- paste0(
            "VA Average\n",
            "Date: ", format(highest_co$report_date, "%D"), "\n",
            "Cases (per 100k): ", floor(highest_co$va_cases), "\n"
        )
        
        text_title <- paste0(
            "Cases in ", highest_co$name[1], " vs. State Average"
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
            title = text_title,
            legend = list(x = 0, y = 1,
                          bgcolor = "transparent",
                          bordercolor = "transparent"),
            yaxis = list(title = "Cases (Per 100k)", showgrid = FALSE,
                         fixedrange = TRUE),
            xaxis = list(title = "", showgrid = FALSE, fixedrange = TRUE,
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
            "Date: ", format(highest_co$report_date, "%D"), "\n",
            "Daily Cases (per 100k): ", floor(highest_co$rate), "\n",
            "7-Day Average (per 100k): ", floor(highest_co$rate.avg), "\n"
        )
        text_va <- paste0(
            "VA Average\n",
            "Date: ", format(highest_co$report_date, "%D"), "\n",
            "Daily Cases (per 100k): ", floor(highest_co$va_rate), "\n",
            "7-Day Average (per 100k): ", floor(highest_co$va_rate.avg), "\n"
        )
        
        text_title <- paste0(
            "Daily Rates in ", highest_co$name[1], " vs. State Average"
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
            title = text_title,
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
    
    ## BY COUNTY PAGE #########################################################
    ## DEMOGRAPHICS PAGE #######################################################
    
    ## Options
    
    # Date Selector
    output$demo_date_ui <- renderUI({
        dateInput("demo_date", "Select Date:",
                  min = sex_min, 
                  max = sex_max,
                  value = sex_max,
                  format = "mm/dd/yy")
    })
    
    # Data Switcher for Population Adjustment
    demo_age_target <- reactive({
        if(input$demo_pop_adj) {
            age.adj
        }
        else {
            age
        }
    })
    
    demo_race_target <- reactive({
        if(input$demo_pop_adj) {
            race.adj
        }
        else {
            race
        }
    })
    
    demo_sex_target <- reactive({
        if(input$demo_pop_adj) {
            sex.adj
        }
        else {
            sex
        }
    })
    
    # Date Variables for Plots
    demo_age_date_sel <- reactive({
        # Prevent errors
        if(length(input$demo_date) == 0) {
            subset(demo_age_target(), report_date == age_max)
        }
        else {
            subset(demo_age_target(), report_date == input$demo_date)
        }
        
    })
    
    demo_race_date_sel <- reactive({
        # Prevent errors
        if(length(input$demo_date) == 0) {
            subset(demo_race_target(), report_date == race_max)
        }
        else {
            subset(demo_race_target(), report_date == input$demo_date)
        }
        
    })
    
    demo_sex_date_sel <- reactive({
        # Prevent errors
        if(length(input$demo_date) == 0) {
            subset(demo_sex_target(), report_date == sex_max)
        }
        else {
            subset(demo_sex_target(), report_date == input$demo_date)
        }
        
    })
    
    # Mode chosen by user
    demo_age_mode_sel <- reactive({
        if(input$demo_pop_adj) {
            switch(input$demo_mode,
                   cases = demo_age_date_sel()$cases.adj,
                   hosp = demo_age_date_sel()$hosp.adj,
                   deaths = demo_age_date_sel()$deaths.adj)
        }
        else {
            switch(input$demo_mode,
                   cases = demo_age_date_sel()$cases,
                   hosp = demo_age_date_sel()$hosp,
                   deaths = demo_age_date_sel()$deaths)
        }
        
    })
    
    demo_race_mode_sel <- reactive({
        if(input$demo_pop_adj) {
            switch(input$demo_mode,
                   cases = demo_race_date_sel()$cases.adj,
                   hosp = demo_race_date_sel()$hosp.adj,
                   deaths = demo_race_date_sel()$deaths.adj)
        }
        else {
            switch(input$demo_mode,
                   cases = demo_race_date_sel()$cases,
                   hosp = demo_race_date_sel()$hosp,
                   deaths = demo_race_date_sel()$deaths)
        }

    })
    
    demo_sex_mode_sel <- reactive({
        if(input$demo_pop_adj) {
            switch(input$demo_mode,
                   cases = demo_sex_date_sel()$cases.adj,
                   hosp = demo_sex_date_sel()$hosp.adj,
                   deaths = demo_sex_date_sel()$deaths.adj)
        }
        else {
            switch(input$demo_mode,
                   cases = demo_sex_date_sel()$cases,
                   hosp = demo_sex_date_sel()$hosp,
                   deaths = demo_sex_date_sel()$deaths)
        }
        
    })
    
    # Plot Customization
    
    demo_age_title <- reactive({
        if(input$demo_pop_adj) {
            switch(input$demo_mode,
                   cases = "Cases by Age\n(Population Adjusted)",
                   hosp = paste0("Hosipitalizations by Age\n",
                                 "(Population Adjusted)"),
                   deaths = "Deaths by Age\n(Population Adjusted)")
        }
        else {
            switch(input$demo_mode,
                   cases = "Cases by Age\n(Total)",
                   hosp = paste0("Hosipitalizations by Age\n",
                                 "(Total)"),
                   deaths = "Deaths by Age\n(Total)")
        }
    })
    
    demo_race_title <- reactive({
        if(input$demo_pop_adj) {
            switch(input$demo_mode,
                   cases = "Cases by Minority\n(Population Adjusted)",
                   hosp = paste0("Hosipitalizations by Minority\n",
                                 "(Population Adjusted)"),
                   deaths = "Deaths by Minority\n(Population Adjusted)")
        }
        else {
            switch(input$demo_mode,
                   cases = "Cases by Minority\n(Total)",
                   hosp = paste0("Hosipitalizations by Minority\n",
                                 "(Total)"),
                   deaths = "Deaths by Minority\n(Total)")
            
        }
    })
    
    demo_sex_title <- reactive({
        if(input$demo_pop_adj) {
            switch(input$demo_mode,
                   cases = "Cases by Sex\n(Population Adjusted)",
                   hosp = paste0("Hosipitalizations by Sex\n",
                                 "(Population Adjusted)"),
                   deaths = "Deaths by Sex\n(Population Adjusted)")
        }
        else {
            switch(input$demo_mode,
                   cases = "Cases by Sex\n(Total)",
                   hosp = paste0("Hosipitalizations by Sex\n",
                                 "(Total)"),
                   deaths = "Deaths by Sex\n(Total)")
            
        }
    })

    ## Plots
    
    # Age Plot
    
    output$demo_age <- renderPlotly({
        plot_ly(
            x = ~demo_age_date_sel()$age_group,
            y = ~demo_age_mode_sel(),
            type = "bar"
            #title = ~demo_age_title()
        )

    })
    
    # Race Plot
    
    output$demo_race <- renderPlotly({
        plot_ly(
            race,
            labels = ~demo_race_date_sel()$race_and_ethnicity,
            values = ~demo_race_mode_sel(),
            type = "pie",
            title = ~demo_race_title()
        )
    })
    
    
    # Sex Plot
    
    output$demo_sex <- renderPlotly({
        plot_ly(
            x = ~demo_sex_date_sel()$sex,
            y = ~demo_sex_mode_sel(),
            type = "bar"
            #title = ~demo_sex_title()
        )
    })
})
