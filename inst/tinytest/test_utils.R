out = toCelsius(c(32, 212)) # freezing point, boiling point

expect_identical(
  out
  , target = c(
    0.
    , 100.
  )
  , info = "output is of the same length as input and content is correct"
)

