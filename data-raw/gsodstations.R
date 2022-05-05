gsodstations = data.table::fread(
  "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
  , colClasses = c(
    rep("character", 6L) # usaf, wban, station name, ctry, state, icao
    , rep("numeric", 3L) # lat, lon, elev(m)
    , rep("character", 2L) # begin, end
  )
  , encoding = "Latin-1"
)

## convert begin, end to `Date`
cols = c(
  "BEGIN"
  , "END"
)

gsodstations[
  , (cols) := lapply(
    .SD
    , as.Date
    , format = "%Y%m%d"
  )
  , .SDcols = cols
]

# sf::st_as_sf(
#   gsodstations
#   , coords = c("LON", "LAT")
#   , crs = 4326
# )
# 
# > Error in st_as_sf.data.frame(gsodstations, coords = c("LON", "LAT"), crs = 4326) : 
# >   missing values in coordinates not allowed

## save to file
usethis::use_data(
  gsodstations
  , overwrite = TRUE
)

## document data
usethis::use_r(
  name = "data"
)
