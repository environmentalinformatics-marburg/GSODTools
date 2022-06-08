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

## replace elevation missing value
gsodstations[
  `ELEV(M)` %in% c(-999.9, -999.0)
  , `ELEV(M)` := NA_real_
]

# ## discard empty strings 
# ## (see `?data.table::fread`: ,"", is unambiguous and read as an empty string)
# idx = sapply(
#   gsodstations
#   , is.character
# )
# 
# cols = names(idx)[idx]
# 
# gsodstations[
#   , (cols) := lapply(
#     .SD
#     , \(x) {
#       x[
#         !nzchar(x)
#       ] = NA_character_
#       
#       return(
#         x
#       )
#     }
#   )
#   , .SDcols = cols
# ]

# sf::st_as_sf(
#   gsodstations
#   , coords = c("LON", "LAT")
#   , crs = 4326
# )
# 
# > Error in st_as_sf.data.frame(gsodstations, coords = c("LON", "LAT"), crs = 4326) : 
# >   missing values in coordinates not allowed

## store internally in R/sysdata.rda --> follows usual export rules
usethis::use_data(
  gsodstations
  , internal = TRUE
  , overwrite = TRUE
)

## write to ./data --> bypass usual export mechanism
usethis::use_data(
  gsodstations
  , overwrite = TRUE
)

## document data
usethis::use_r(
  name = "data"
)
