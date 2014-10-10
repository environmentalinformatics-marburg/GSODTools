GSODTools
=========

### Global options

To be honest, I have no idea how to hide the following lines of code that take 
care of setting some global options. The file README.rmd is generated using the
`knitr` package, and it works out fine calling 'Knit HTML' from within RStudio, 
i.e. code chunks defined as `include = FALSE` are not displayed. However, when I 
upload the file to GitHub, the package's front page shows all code chunks in 
README.rmd, both `include = TRUE` and `include = FALSE`. Any help on that would 
be highly appreciated. Anyway, just ignore the following except 
you are interested in setting some global options.



### What it is all about

But to return to the actual topic: Every person dealing with long-term 
climatological data (e.g. of daily air temperature, relative humidity, and precipitation amounts) will sooner or later stumble across the Global Summary Of Day (GSOD) climate data collection provided by the National Oceanic and Atmospheric Association (NOAA). I've been 
recently looking for available GSOD stations in close vicinity to Mt. Kilimanjaro, Tanzania, and as I am trying to realize most of my coding work using R, I quickly 
noticed that there are only a few packages that provide convenient tools for 
processing GSOD data. Therefore, I started to write this package that includes both downloading data sets of selected climate stations for a given time span as well as 
some processing steps for quality assurance and gap filling.


### Introducing the processing chain

**Getting started**

The starting point for each GSOD-related search query is the selection of a 
particular station (or even multiple stations). Although a [GIS Data Locator][1] exists that allows interactive 
station selection and data acquisition, I thought it was a good thing to 
implement a couple of search function to speed things up a little bit. 

The **GSODTools** package comes with a built-in dataset from [NOAA's FTP server][2]
holding information about all available GSOD stations that is automatically 
attached via lazy-loading when loading the package. Let's have a quick look at it. 


```
##   USAF  WBAN STATION.NAME CTRY FIPS STATE CALL   LAT   LON ELEV..1M.    BEGIN      END
## 1 6852 99999         SENT   SW   SZ            46817 10350     14200       NA       NA
## 2 7005 99999   CWOS 07005                         NA    NA        NA 20120127 20120127
## 3 7010 99999   CWOS 07010                         NA    NA        NA       NA       NA
## 4 7011 99999   CWOS 07011                         NA    NA        NA 20111025 20121129
## 5 7012 99999   CWOS 07012                         NA    NA        NA       NA       NA
## 6 7015 99999   CWOS 07015                         NA    NA        NA       NA       NA
```

Unfortunatelly, the data formatting and consistency of this official table is 
rather poor. Hence, I thought it might be quite helpful to sort out some 
inconveniences above all else. The referring function is called *gsodReformat()* 
and allows to reformat elevation (decimeters to meters) as well as latitude and longitude
(thousandth of a degree to whole degree). Furthermore, it offers the opportunity
to remove invalid coordinates, i.e. values of latitude and longitude exceeding 
valid thresholds of +/-90 and +/-180 degrees, respectively. Optionally, the 
adjusted dataset can be converted to an object of class `sp` prior to return.

Consequently, the first lines of code working with **GSODTools** should probably 
look like this.


```r
# Reformat data and convert to spatial object
gsod_shp <- gsodReformat(data = gsodstations,
                         elevation = TRUE, 
                         coords = TRUE,
                         df2sp = TRUE)

par(mar = c(0, 0, 0, 0))
plot(gsod_shp)
```

![plot of chunk gsodReformat](Figs/gsodReformat.png) 

**Selecting a station**

Now that the list of available GSOD stations is in a reasonable format and holds
spatial information, the next step would be to select a station you would like to
download data from. Using the GIS Data Locator, this involves quite some clicking 
around until you finally reach the download page. **GSODTools** offers multiple 
functions to facilitate station selection and data acquisition, allowing the user
to select stations based on spatial characteristics or by hand. 

`stationFromCoords` takes a x (longitude) and y (latitude) coordinate as input, 
and returns all available GSOD stations that fall within a user-defined buffer
around that location. Alternatively, a 'SpatialPoints' object may be provided 
rather than two separate numerics. For instance, let's search for GSOD stations
in a circle of 500 km around Kibo summit, Mt. Kilimanjaro, Tanzania. The referring
coordinates are `c(37.359031, -3.065053)`.


```r
shp_kibo <- stationFromCoords(x = 37.359031, y = -3.065053, width = 500)
# or: stationFromCoords(x = c(37.359031, -3.065053), width = 500)
# or: stationFromCoords(x = SpatialPoints(data.frame(x = 37.359031, 
#                                                    y = -3.065053), 
#                                         proj4string = CRS("+init=epsg:4326")), 
#                       width = 500)

mapGriddedData(mapRegion = "africa", plotData = FALSE, borderCol = "black",
               addLegend = FALSE)
points(shp_kibo, col = "red", pch = 20, cex = 2)
```

![plot of chunk stationFromCoords](Figs/stationFromCoords.png) 

`stationFromExtent`, just like `stationFromCoords`, allows station selection 
based on spatial criteria. However, the user is prompted to manually draw an 
extent on a map rather than directly supplying specific coordinates. The advantage 
is that spatial selection is not performed in a circular shape, i.e. in a uniform
distance around a given location, but depends on user preferences. With respect
to the aforementioned example, this means that GSOD stations in the southern
Mt. Kilimanjaro region could be selected rather than all stations in a given 
distance from the summit. Alternatively, a rectangular bounding box can be 
supplied instead of calling `drawExtent` (which is actually quite difficult to
include in a README file). 


```r
bbox_kibo_south <- extent(c(36.6, 37.72, -3.5, -3.065053))
shp_kili_south <- stationFromExtent(bb = bbox_kibo_south)

mapGriddedData(mapRegion = "africa", plotData = FALSE, borderCol = "black",
               addLegend = FALSE)
points(shp_kili_south, col = "red", pch = 20, cex = 2)
```

![plot of chunk stationFromExtent](Figs/stationFromExtent.png) 

The third and, at the moment, final possibility to select a GSOD station is to 
simply choose a name from the built-in station list. This is, however, a quite
tricky approach since you have to know the precise spelling of a station's name. 
Again referring to the above example where we selected Arusha, Moshi, and 
Kilimanjaro International Airport (KIA), this would more or less look like this. 


```r
library(dplyr)

station_names <- c("ARUSHA", "KILIMANJARO AIRPORT", "MOSHI")

shp_kili_south <- 
  gsodstations %>% 
  gsodReformat() %>% 
  filter(STATION.NAME %in% station_names) %>% 
  gsodDf2Sp()

shp_kili_south@data
```

```
##     USAF  WBAN        STATION.NAME CTRY FIPS STATE CALL ELEV..1M.    BEGIN      END
## 1 637890 99999              ARUSHA   TN   TZ       HTAR      1387 19600111 20130705
## 2 637900 99999               MOSHI   TN   TZ       HTMS       831 19490909 20130612
## 3 637910 99999 KILIMANJARO AIRPORT   TN   TZ       HTKJ       896 19730101 20130705
```

**Downloading data**

Ideally, you have now found a 

[1]: http://www.climate.gov/daily-observational-data-global-summary-day-gsod-%E2%80%93-gis-data-locator
[2]: ftp://ftp.ncdc.noaa.gov/pub/data/gsod/ish-history.csv
