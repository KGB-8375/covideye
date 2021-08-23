## Main server-side app

# Check cached data integrity
if(!file.exists("DATA/pop.csv")) {
    source("getData.R")
}

if(!file.exists("DATA/temp/covid_local.csv")) {
    source("update.R")
}

# Update old data (should be less than 1 day old)
if(file.info("DATA/temp/covid_local.csv")$mtime < Sys.time() - as.difftime(1, unit = "days")) {
    source("update.R")
}

# Read cached data tables
covid.local <- fread("DATA/temp/covid_local.csv")
covid.confd <- fread("DATA/temp/covid_confd.csv")
covid.age   <- fread("DATA/temp/covid_age.csv")
covid.race  <- fread("DATA/temp/covid_race.csv")
covid.sex   <- fread("DATA/temp/covid_sex.csv")
vaccn.local <- fread("DATA/temp/vaccn_local.csv")
pop         <- fread("DATA/pop.csv")
local       <- readOGR(
    dsn   = "./DATA/shapefile",
    layer   = "cb_2019_us_county_500k",
    verbose = FALSE
)

# Prepare data

## Locality data (Spatial data file)
# This is done here because it mixes static and fresh data & is useful
# for several modules
pop.local <- pop %>%
    group_by(fips) %>%
    summarize(pop = sum(pop))

local <- local %>%
    # Only VA
    filter(STATEFP == "51") %>%
    # Extract fips (other data isn't useful)
    transmute(fips = as.integer(GEOID)) %>%
    inner_join(covid.local, by = "fips") %>%
    inner_join(vaccn.local, by = c("fips", "date", "local")) %>%
    inner_join(pop.local,   by = "fips") %>%
    # Create population adjusted vars
    mutate(
        total.c.adj  = total.c  * 100000 / pop,
        total.h.adj  = total.h  * 100000 / pop,
        total.d.adj  = total.d  * 100000 / pop,
        total.1d.adj = total.1d * 100000 / pop,
        total.fv.adj = total.fv * 100000 / pop,
        rate.c.adj   = rate.c   * 100000 / pop,
        rate.h.adj   = rate.h   * 100000 / pop,
        rate.d.adj   = rate.d   * 100000 / pop,
        rate.1d.adj  = rate.1d  * 100000 / pop,
        rate.fv.adj  = rate.fv  * 100000 / pop
    )

rm(covid.local, pop.local, vaccn.local)

# Main server functionality for website
function(input, output, session) {
    dashboardServer("dashboard", local, covid.confd)
    byCountyServer("byCounty")
    demographicsServer("demographics", covid.age, covid.race, covid.sex, pop)
    
    # Theme
    observeEvent(input$dark_mode, {
        session$setCurrentTheme(
            if (input$dark_mode)
                bs_theme_update(dark)
            else
                bs_theme_update(light)
        )
    })
    
    # Navigation
    autoNavigating <- reactiveVal(0)
    pageLoading    <- reactiveVal(TRUE)
    
    # Restore the Shiny app's state
    restore <- function(qs) {
        data <- parseQueryString(qs)
        nav <- FALSE
        
        if(!is.null(data['page'])) {
            nav <- TRUE
            
            updateTabsetPanel(session, "navbar", data[['page']])
        }
        
        if(!is.null(data['dark'])) {
            nav <- TRUE
            
            updateSwitchInput(session, "dark_mode", as.logical(data[['dark']]))
        }
        
        if(nav) {
            autoNavigating(autoNavigating() + 1)
        }
    }
    
    # Navigate to URL requested on page load
    observeEvent(session$clientData$url_search, {
        if(!pageLoading()) return()
        pageLoading(FALSE)
        
        if(nchar(session$clientData$url_search) > 1) {
            autoNavigating(autoNavigating() + 1)
            restore(session$clientData$url_search)
        }
    })
    
    # Save page state to URL
    observeEvent(list(input$navbar, input$dark_mode), {
        if(autoNavigating() > 0) {
            autoNavigating(autoNavigating() - 1)
            return()
        }
        
        pageLoading(FALSE)
        
        js$updateHistory(page = input$navbar, dark = input$dark_mode)
    })
    
    # Restore state on next/prev button presses
    observeEvent(input$navigatedTo, {
        restore(input$navigatedTo)
    })
}
