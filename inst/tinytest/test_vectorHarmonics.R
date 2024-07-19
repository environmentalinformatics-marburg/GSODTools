dat = subset(
  eastafrica
  , PlotId == "kil" &
    Datetime < "1984-01-01"
)

agg = aggregate(
  dat["TEMP"]
  , by = list(
    YEARMO = format(
      dat$Datetime
      , "%Y-%m"
    )
  )
  , FUN = mean
  , na.rm = TRUE
)

out = vectorHarmonics(
  agg$TEMP
  , st = c(1980, 1)
  , nd = c(1983, 12)
)

expect_inherits(
  out
  , class = "numeric"
)

expect_identical(
  length(out)
  , target = formals(
    vectorHarmonics
  )$frq
  , info = "output is of the same length as input frequency"
)
