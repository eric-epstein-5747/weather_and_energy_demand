library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)

## 1. Create a table containing electricity demand data for Washington, DC----
# Obtain Retail sales of electricity in Washington, DC in millions of kilowatt hours, 
# by month and sector, from 2001 to 2019, from the following site:
# https://www.eia.gov/electricity/data/browser/#/topic/5?agg=0,1&geo=0000002&endsec=vg&freq=M&start=200101&end=201908&ctype=linechart&ltype=pin&rtype=s&pin=&rse=0&maptype=0 
  
retail_sales <- read_csv("dc_retail_sales_elec_million_kilowatthrs.csv", na="--")

# Inspect retail_sales:
head(retail_sales)
tail(retail_sales)

# Note: Dates here are in format "1-Jan", representing January 1, 2001. This format is unusual; let's
# create some more parseable year and month columns (to join on later):
retail_sales$year <- as.character(as.numeric(str_replace(retail_sales$date, "([0-9]*)-([A-Za-z]{3})", "\\1")) + 2000)
retail_sales$month <- str_replace(retail_sales$date, "[0-9]*-([A-Za-z]{3})", "\\1")
# NOTE: Each month is represented here by the first 3 letters of its English name.

# Select out the variables we're going to look at
retail_sales <- retail_sales %>% select(year,month,all_sectors,commercial,industrial,other,residential,transportation)


## 2. Create a table containing weather data for Washington, DC----
# Obtain daily weather data at National Arboretum station in Washington, DC, from 2001 to 2019: 
# https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00186350/detail
# Having inspected this data, we select only the columns we are likely to have interest in:
weather <- read_csv("dc_weather_2001-2019.csv") %>% select(DATE,PRCP,SNOW,SNWD,TMAX,TMIN)
colnames(weather) <- tolower(colnames(weather))

# Create year and month variables to join on later (to match columns in electricity demand data)
weather$year <- as.character(year(weather$date))

# So that we can join weather data to sales data, create some functions to switch between 
# numerical & character representations of months:

months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

f <- function (num) {
  return (months[num])
}

g <- function (name) {
  return (match(name,months))
}

weather$month <- f(month(weather$date))
# Ok good. Now if we aggregate by month we can join the weather data to the electricity data.

# We should have data for 227 months (from Jan 2001 to Nov 2019). Inspecting this data, we find
# we're missing 16 months:
unique(weather[c("month", "year")]) %>% group_by(year) %>% count()
# We're missing December 2001, Jan - Nov of 2002, and Sep - Nov 2019.

# So, let's supplement the data with some data from a nearby station, at the Baltimore Washington 
# International Airport (BWI). This will give us the added advantage of stabilizing the data against
# particularities of the micro-climate around the Arboretum station, since we'll now aggregate data 
# from two different stations.
#
# Save the weather from the National Arboretum in a separate dataframe
weather_arb <- weather
#
# Obtain weather data from the BWI station:
# https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00093721/detail
weather_bwi <- read_csv("weather_bwi_2001-2019.csv") %>% select(DATE,PRCP,SNOW,SNWD,TMAX,TMIN)
colnames(weather_bwi) <- tolower(colnames(weather_bwi))

# Create year and month variables to join on later
weather_bwi$year <- as.character(year(weather_bwi$date))
weather_bwi$month <- f(month(weather_bwi$date))

# Check if any months are missing:
unique(weather_bwi[c("month", "year")]) %>% group_by(year) %>% count()
# Great, no missing months.

# Glue our two weather datasets together: 
weather <- rbind(weather_bwi, weather_arb)

## 3. Aggregate the weather data so we can join it with the sales data----
# NOTE: here we want an aggregation method that's sensitive to outliers, since outliers may affect
# energy demand.
weather_means <- weather %>% 
    group_by(year,month) %>% 
      summarise(prcp=mean(prcp, na.rm=TRUE),snow=mean(snow,na.rm=TRUE),snwd=mean(snwd,na.rm=TRUE),
                tmax=mean(tmax,na.rm=TRUE),tmin=mean(tmin,na.rm=TRUE))


## 4. Join the weather data to the energy data so we can start looking for patterns----
sales_weather <- left_join(retail_sales, weather_means, by=c("year","month"))
sales_weather$nmonth <-  g(sales_weather$month)

## 5. Export the joined data to the project directory so we don't have to recreate it for each session----
write_csv(sales_weather, "sales_weather.csv", col_names=TRUE)