## `stationFromCoords()` ====

## 'numeric' input
gsod_shp = stationFromCoords(
  x = c(
    37.359031
    , -3.065053
  )
  , width = 100
)

expect_inherits(
  gsod_shp
  , class = "sf"
)

## 'sf' input
kibo = sf::st_as_sf(
  data.frame(
    x = 37.359031
    , y = -3.065053
  )
  , crs = 4326
  , coords = c("x", "y")
)

gsod_shp1 = stationFromCoords(
  kibo
  , width = 100
)

expect_identical(
  gsod_shp1
  , target = gsod_shp
  , info = "'numeric' (ie coordinates) and 'sf' input give identical results"
)

expect_true(
  all(
    setdiff(
      names(gsodstations)
      , c("LAT", "LON")
    ) %in% names(gsod_shp)
  )
  , info = "all names from built-in data expect for lat, lon are preserved"
)

expect_true(
  "DIST" %in% names(gsod_shp)
  , info = "in case of coords, 'DIST' column is appended to output"
)


## `stationFromExtent()` ====

kili = sf::st_bbox(
  c(
    xmin = 37
    , xmax = 37.72
    , ymin = -3.4
    , ymax = -2.84
  )
)

gsod_shp_kili = stationFromExtent(
  bb = kili
)

expect_inherits(
  gsod_shp_kili
  , class = "sf"
)

expect_true(
  all(
    setdiff(
      names(gsodstations)
      , c("LAT", "LON")
    ) %in% names(gsod_shp_kili)
  )
  , info = "all names from built-in data expect for lat, lon are preserved"
)

expect_false(
  "DIST" %in% names(gsod_shp_kili)
  , info = "in case of extent, 'DIST' column is not appended to output"
)
