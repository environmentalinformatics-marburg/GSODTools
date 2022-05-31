library(tinytest)
library(checkmate)
using("checkmate")


## STRUCTURE ====

expect_data_table(
  gsodstations
  , ncols = 11L
)


## CONTENT ====

### elevation ----

gsodstations[
  , expect_numeric(
    `ELEV(M)`
    # https://www.universetoday.com/15027/lowest-point-on-earth/
    , lower = -420
    # https://geology.com/records/highest-mountain-in-the-world.shtml
    , upper = 8848.86
  )
]
