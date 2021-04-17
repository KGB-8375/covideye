# Automatically retrieve all temporary data needed for website

library(RSocrata)   # Reading data from socrata sites (VDH)
library(data.table) # Write data frame to .csv

# Fetch data
cases <- read.socrata("https://data.virginia.gov/resource/bre9-aqqr.json")
confd <- read.socrata("https://data.virginia.gov/resource/uqs3-x7zh.json")
pop   <- read.socrata("https://data.virginia.gov/resource/5s4f-hthh.json")
age   <- read.socrata("https://data.virginia.gov/resource/uktn-mwig.json")
race  <- read.socrata("https://data.virginia.gov/resource/9sba-m86n.json")
sex   <- read.socrata("https://data.virginia.gov/resource/tdt3-q47w.json")

# Cache data as .csv
fwrite(cases, "DATA/temp/cases.csv")
fwrite(confd, "DATA/temp/confd.csv")
fwrite(pop,   "DATA/temp/pop.csv")
fwrite(age,   "DATA/temp/age.csv")
fwrite(race,  "DATA/temp/race.csv")
fwrite(sex,   "DATA/temp/sex.csv")
