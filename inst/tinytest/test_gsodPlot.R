## sample data
lst = split(
  eastafrica
  , f = paste(
    eastafrica$PlotId
    , eastafrica$Status
    , sep = ","
  )
)

cleansed_data = lst[
  grep(
    "cleansed"
    , names(lst)
  )
]

filled_data = lst[
  grep(
    "filled"
    , names(lst)
  )
]

expect_inherits(
  gsodPlot(
    cleansed_data
    , filled_data
    , stations = c("NAIROBI JKIA", "KILIMANJARO INTL")
    , type = "trends"
  )
  , class = "ggplot"
)

## early exit: non-matching type
expect_error(
  gsodPlot(
    cleansed_data
    , filled_data
    , stations = c("NAIROBI JKIA", "KILIMANJARO INTL")
    , type = "smoothed"
  )
  , info = "'arg' should be one of"
)
