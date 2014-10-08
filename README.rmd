GSODTools
=========

```{r global_options, echo = FALSE}
opts_chunk$set(fig.width = 12, fig.height = 8, fig.path = 'Figs/',
               include = TRUE, warning = FALSE, message = FALSE)
```

### What it is all about

Every person dealing with long-term climatological data (e.g. of daily air
temperature, relative humidity, and precipitation amounts) will sooner or later
stumble across the Global Summary Of Day (GSOD) climate data collection 
provided by the National Oceanic and Atmospheric Association (NOAA). In the 
course of my PhD thesis, I've been recently looking for available GSOD 
stations in close vicinity to Mt. Kilimanjaro, Tanzania, and as I am trying to
realize most of my coding work using R, I quickly noticed that there are only a 
few packages that provide convenient tools for processing GSOD data. Therefore, 
I started to write this package that includes both downloading data sets of 
selected climate stations for a given time span as well as 
some processing steps for quality assurance and gap filling.


### Introducing the processing chain

**Getting started**

The starting point for each GSOD-related search query is the selection of a 
particular station (or even multiple stations). Although a [GIS Data Locator][1] exists that allows interactive 
station selection and data acquisition, I thought it was a good thing to 
implement a couple of search function to speed things up a little bit. 

The `GSODTools` package comes with a built-in dataset from [NOAA's FTP server][2]
that is automatically attached via lazy-loading when loading the package. Let's 
have a quick look at it. 

```{r gsodstations, echo = FALSE}
head(gsodstations)
```

After loading the package and the corresponding data, I thought it might be quite
useful to optionally sort out some inconveniences in the GSOD station list. The
referring function is called *gsodReformat()*. The function includes the possibilities
to reformat the following columns:
- elevation: initially in dm, transferred to m if `elevation = TRUE` (default)
- coordinates: initially in decimal degrees / 1000, transferred to 'usual' 
decimal degrees if `coords = TRUE` (default); in addition, all invalid coordinates, 
i.e. longitude < -180° or > 180° and latitude < -90° or > 90° degree, are 
repliced with NA.
- the supplied data.frame can be converted into a `SpatialPointsDataFrame` if
`df2sp = TRUE` (default is `df2sp = FALSE`)

Hence, the first lines of code using **eimarGsodTools** should probably look like this:

```S
# Load package and related data
library(eimarGsodTools)
data(gsodstations)

# Reformat data and convert to spatial object
stations.sp <- gsodReformat(data = gsodstations,
                            elevation = TRUE, 
                            coords = TRUE,
                            df2sp = TRUE)
```

**Downloading data**


[1]: http://www.climate.gov/daily-observational-data-global-summary-day-gsod-%E2%80%93-gis-data-locator
[2]: ftp://ftp.ncdc.noaa.gov/pub/data/gsod/ish-history.csv