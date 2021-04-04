## Import Libraries ############################################################
library(shiny)
library(RSocrata)
library(rgdal)
library(leaflet)
library(dplyr)
library(sp)
## Static information (Doesn't change until app restarted) #####################

# Import datasets
covid_dat <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json")

spdf <- readOGR(
  dsn = "./app/DATA/shapefile",
  layer = "cb_2019_us_county_500k"
)

# Convert data to correct types
covid_dat$report_date      <- as.Date(covid_dat$report_date)
covid_dat$total_cases      <- as.numeric(covid_dat$total_cases)
covid_dat$deaths           <- as.numeric(covid_dat$deaths)
covid_dat$hospitalizations <- as.numeric(covid_dat$hospitalizations)

# Get just the data for VA
spdf <- subset(spdf, STATEFP == "51")

# Now kiss (merge spatial data with covid data)
# This uses a relatively large (~280 mb) amount of memory
spdf <- merge(spdf, covid_dat, 
              by.x = "GEOID", by.y = "fips", 
              duplicateGeoms = T)

# Create custom color bins
# Old bins were based on number, new one is %
#mybins <- c(0, 1000, 2000, 5000, 10000, 20000, 40000, 60000, Inf)
mybins <- c(0, 1.5625, 3.125, 6.25, 12.5, 25, 50, 75, Inf) / 100

# Set minimum and maximum selection dates
max_date <- tail(covid_dat$report_date, n = 1)
min_date <- head(covid_dat$report_date, n = 1)

## UI Information  (HTML Page) #################################################

ui <- fluidPage(
  # Title
  titlePanel("RVA CovidView", windowTitle = "RVA CovidView"),
  
  # Get Requested date for data retrieval
  dateInput(inputId = "date", label = "Input date:",
            value = max_date,
            min = min_date, max = max_date),
  
  # Draw a choropleth map for that date
  leafletOutput("choropleth")
)

## Server Section (Functionality) ##############################################
server <- function(input, output) {
  # Create a choropleth
  output$choropleth <- renderLeaflet({
    # First we need to calculate all the data for the leaflet
    
    # Get data subset for date
    selection <- subset(spdf, report_date == input$date)
    
    # Generate bins based on percentages
    # Only use 3 sig figs & whole numbers
    bins_f <- unique(ceiling(signif(mybins * max(selection$total_cases),
                                    digits = 3)))
    
    # Create color palette
    mypalette <- colorBin(palette = "YlOrRd", 
                          domain = selection$total_cases,
                          na.color = "black",
                          bins = bins_f)
    
    # Generate tooltip text
    mytext <- paste(
      "<b>", selection$locality, "</b></br>",
      "Covid Cases: ", selection$total_cases, "</br>",
      "Hospitilizations: ", selection$hospitalizations, "</br>",
      "Deaths: ", selection$deaths, "</br>",
      sep = ""
    ) %>% lapply(htmltools::HTML)
    
    # Finally, create actual choropleth using Leaflet
    leaflet(selection) %>%
      # Crop view to just show VA
      fitBounds( lat1 = 36.585007, lng1 = -83.714340,
                 lat2 = 39.447387, lng2 = -74.869230) %>%
      # Add data to the screen
      addPolygons(
        # Get color
        fillColor = ~mypalette(selection$total_cases),
        # Add thin border
        stroke = T,
        # Make tiles opaque
        fillOpacity = 1.0,
        # Make border white
        color = "grey",
        # Make border skinny
        weight = 0.6,
        # Add tooltip info
        label = mytext,
        # Change tooltip styling
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      # And lastly add a legend to know what each color means
      addLegend( pal=mypalette, values=selection$total_cases, opacity = 0.9,
                 title = "Total Covid Cases", position = "topleft" )
  })
}

# Create our app given the UI and Server elements
shinyApp(ui = ui, server = server)
