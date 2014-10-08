GSODTools
=========

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
some processing steps (detection of statistic outliers, gap-filling based upon
linear interpolation, linear models and singular spectrum analysis (SSA)) for 
quality assurance.


### Introducing the processing chain

I am currently struggling with loading sample data automatically into the R
workspace. Any kind of advice on that would be highly appreciated. The ASCII file 
containing the information about all the single GSOD stations around the world 
is absolutely necessary in order to find a desired location for data download, 
and right now, I see now other way than importing it manually by calling `data()`.


**Getting started**

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
