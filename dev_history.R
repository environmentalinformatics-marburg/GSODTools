# 2021-07-29 ====

library(GSODTools)

## stations update
isd_history = curl::curl_download(
  "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
  , destfile = tempfile(
    fileext = ".csv"
  )
)
# file.info(
#   isd_history
# )

gsodstations = read.csv(
  isd_history
)

usethis::use_data(
  gsodstations
  , overwrite = TRUE
)

## issue #4
shp_kibo = stationFromCoords(
  x = 37.359031
  , y = -3.065053
  , width = 500
)

mapview::mapview(
  shp_kibo
)


# 2021-11-22 ====

gsodstations = read.csv(
  "https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv"
)

usethis::use_data(
  gsodstations
  , overwrite = TRUE
)


# 2022-04-26 ====

# in response to https://github.com/environmentalinformatics-marburg/GSODTools/issues/5
gsod_shp = gsodReformat(
  data = gsodstations
  , elevation = TRUE
  , coords = TRUE
  , rm_invalid_coords = TRUE
  , df2sp = TRUE
)


# 2022-05-06 ====

# combines data sets in './data/data_nairobi_kilimanjaro.RData' into one 
# (original file to be deleted after built-in combined data is ready)

data("data_nairobi_kilimanjaro")

dfs = ls(
  pattern = "^df_"
)

eastafrica = lapply(
  dfs
  , \(i) {
    
    # create preprocess status depending on data set name:
    # * cleansed = outliers removed
    # * filled = gaps filled
    status = ifelse(
      grepl("gsod", i)
      , "cleansed"
      , "filled"
    )
    
    tmp = eval(
      parse(
        text = i
      )
    )
    
    transform(
      tmp
      # timestamps to 'POSIXt'
      , Datetime = as.POSIXct(
        Datetime
        , tz = "EAT"
      )
      # append preprocess status
      , "Status" = status
    )
  }
) |> 
  data.table::rbindlist() |> 
  transform(
    Status = factor(Status)
  ) |> 
  # reorder by station name, preprocess status
  data.table::setorderv(
    cols = c(
      "PlotId"
      , "Status"
    )
  )

## drop columns with a single unique entry
uniques = eastafrica[
  , lapply(
    .SD
    , data.table::uniqueN
  )
] == 1L

eastafrica = eastafrica[
  , .SD
  , .SDcols = colnames(uniques)[!uniques]
]

## rearrange columns
data.table::setcolorder(
  eastafrica
  , c(
    "PlotId"
    , "Status"
    , "Datetime"
  )
)

## save to file
usethis::use_data(
  eastafrica
  , overwrite = TRUE
)

## document data
usethis::use_r(
  name = "data"
)


# 2022-06-02 ====

## Find stations with null island coordinates (ie lat = lon = 0.0) in built-in 
## `gsodstations` and, if country code is assigned, intersect country boundaries
## with zero point (spoiler: no intersection)


### country list ----

## download data
destfile = file.path(
  tempdir()
  , "country-list.txt"
)

utils::download.file(
  "https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/country-list.txt"
  , destfile = destfile
  , method = "wget"
  , quiet = TRUE
)

## read data
lns = readLines(
  destfile
)

fips_ids = utils::read.fwf(
  destfile
  , skip = 2L
  , header = FALSE
  , widths = c(
    12L # fips id
    , max(nchar(lns)) - 12L # country name
  )
) |> 
  data.table::data.table()

## set header
names(
  fips_ids
) = strsplit(
  lns[1]
  , "\\s{2,}"
)[[1]]

## strip away trailing whitespace
fips_ids = fips_ids[
  , lapply(
    .SD
    , \(i) {
      gsub("\\s*$", "", i)
    }
  )
]


### null island stations ----
### (see https://en.wikipedia.org/wiki/Null_Island)

## select stations at null island that do have country indication
sbs = GSODTools::gsodstations[
  LON == 0 & 
    LAT == 0 & 
    nzchar(
      CTRY
    )
]

## split by country code
lst = split(
  sbs
  , by = "CTRY"
)

## discard stations with invalid country code
ivl = setdiff(
  names(lst)
  , fips_ids$`FIPS ID`
)

lst[
  ivl
] = NULL


### intersection ----

null_island = data.frame(
  lon = 0
  , lat = 0
) |> 
  sf::st_as_sf(
    crs = 4326
    , coords = c("lon", "lat")
  )

## cycle through null island fips ids
intersections = Map(
  \(i) {
    
    # convert to iso3
    iso3 = fips_ids[
      `FIPS ID` == i
      , `COUNTRY NAME`
    ] |> 
      rworldmap::rwmGetISO3()
    
    # download gadm data
    aoi = raster::getData(
      country = iso3
      , path = tempdir()
      , level = 0L
    ) |> 
      sf::st_as_sf()
    
    # test intersection
    sf::st_intersects(
      aoi
      , null_island
    )
  }
  , names(lst)
)
