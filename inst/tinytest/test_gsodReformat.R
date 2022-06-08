expect_warning(
  dat <- gsodReformat(
    gsodstations
  )
  , pattern = "'gsodReformat' is deprecated.\nUse 'gsodDf2Sp' instead."
)

expect_identical(
  dat
  , target = gsodstations
  , info = "returns unaltered input if `df2sp = FALSE` (default)"
)

expect_inherits(
  suppressWarnings(
    gsodReformat(
      gsodstations
      , df2sp = TRUE
    )
  )
  , class = "sf"
)
