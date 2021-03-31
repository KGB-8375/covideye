## Import Libraries ############################################################
library(shiny)
library(RSocrata)
library(geojsonio)
library(leaflet)
## Static information (Doesn't change until app restarted) #####################

# Import datasets
covid_dat <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json",
                          app_token = Sys.getenv("SOCRATA_TOKEN"))
geo_dat <- geojson_read("https://opendata.arcgis.com/datasets/e3c8822a4adc4fc1a542a233893a46d4_0.geojson", 
                               what = "sp")

# Create custom color bins
mybins <- c(0, 1000, 2000, 5000, 10000, 20000, 40000, 60000, Inf)

# Set minimum and maximum selection dates
max_date <- as.character(tail(covid_dat$report_date, n = 1))
min_date <- as.character(head(covid_dat$report_date, n = 1))

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
    dated_dat <- subset(covid_dat, as.Date(report_date) == input$date)
    
    # Get data for cases (FIPS sorted)
    dated_cases <- as.integer(dated_dat$total_cases[order(match(dated_dat$fips, geo_dat$GEOID))])
    # Get data for hospitilizations (FIPS sorted)
    dated_hospt <- as.integer(dated_dat$hospitalizations[order(match(dated_dat$fips, geo_dat$GEOID))])
    # Get data for deaths (FIPS sorted)
    dated_death <- as.integer(dated_dat$deaths[order(match(dated_dat$fips, geo_dat$GEOID))])
    
    # Create color palette
    mypalette <- colorBin(palette = "YlOrRd", domain = dated_cases,
                          na.color = "black", bins=mybins )
    
    # Generate tooltip text
    mytext <- paste(
      "County: ", geo_dat$NAMELSAD, "</br>",
      "Covid Cases: ", dated_cases, "</br>",
      "Hospitilizations: ", dated_hospt, "</br>",
      "Deaths: ", dated_death, "</br>",
      sep = ""
    ) %>% lapply(htmltools::HTML)
    
    # Finally, create actual choropleth using Leaflet
    leaflet(geo_dat) %>%
      # Set default view location & zoom level
      setView( lat = 37.4316, lng = -79.6569, zoom = 6) %>%
      # Add data to the screen
      addPolygons(
        # Get color
        fillColor = ~mypalette(dated_cases),
        # Add thin border
        stroke = T,
        # Make tiles opaque
        fillOpacity = 1.0,
        # Make border white
        color = "white",
        # Make border skinny
        weight = 0.3,
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
      addLegend( pal=mypalette, values=dated_cases, opacity = 0.9,
                 title = "Total Covid Cases", position = "bottomleft" )
  })
}

# Create our app given the UI and Server elements
shinyApp(ui = ui, server = server)
