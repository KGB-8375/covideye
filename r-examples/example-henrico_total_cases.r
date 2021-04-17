# COVID Challenge Data

library("RSocrata")

#confirmed <- read.socrata("https://data.virginia.gov/resource/uqs3-x7zh.json")

# Get data from VDH website
countyData <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json")
# create subset of only henrico's data
countyData.henrico <- subset(countyData, countyData$fips == 51087)
# Graph total cases by report date
plot(x = countyData.henrico$report_date,
     y = countyData.henrico$total_cases,
     ylab = "Confirmed Cases",
     xlab = "Date",
     type = "l", # use a line plot
     main = "Total Cases in Henrico, VA" )
