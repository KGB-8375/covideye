# Get covid data
library("RSocrata")
covid_dat <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json")

# Get most recent data (end of dataset)
new_dat <- subset(covid_dat, report_date == tail(covid_dat, n=1)$report_date)

# Get shapefile of VA
library("geojsonio")
spdf = geojson_read("https://opendata.arcgis.com/datasets/e3c8822a4adc4fc1a542a233893a46d4_0.geojson", 
                    what = "sp")

# Correct FIPS codes to be complete
spdf$fips <- paste0(spdf$STATEFP, spdf$COUNTYFP)

# Get data from county FIPS codes
disp_dat <- new_dat$total_cases[order(match(new_dat$fips, spdf$fips))]
disp_dat <- as.integer(disp_dat)

# Create a color palette for bins
library(leaflet)

mybins <- c(0, 1000, 2000, 5000, 10000, 20000, 40000, 60000, Inf)
mypalette <- colorBin( palette = "YlOrRd", domain = disp_dat, 
                       na.color = "black", bins=mybins)

# Prepare text for tooltips
mytext <- paste(
  "County: ", spdf@data$NAMELSAD, "</br>",
  "Covid Cases: ", disp_dat, "</br>",
  sep = ""
) %>% lapply(htmltools::HTML)

# Draw Choroplast
m <- leaflet(spdf) %>%
  #addTiles() %>% # Add default map background
  setView( lat = 37.4316, lng = -79.6569, zoom = 6) %>%
  addPolygons(
    fillColor = ~mypalette(disp_dat),
    stroke = T,
    fillOpacity = 1.0,
    color = "white",
    weight = 0.3,
    label = mytext,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=disp_dat, opacity = 0.9,
             title = "Total Covid Cases", position = "bottomleft" )

m

library(htmlwidgets)
saveWidget(m, file="choropleth.html")
