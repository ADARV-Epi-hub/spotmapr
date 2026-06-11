test_that("all_cases = TRUE builds a map with no outcome column", {
  df <- data.frame(
    fever    = c("yes", "yes", "no",  "yes"),
    diarrhea = c("no",  "yes", "yes", "no"),
    lat      = c(11.0, 11.1, 11.2, 11.3),
    lon      = c(76.9, 77.0, 77.1, 77.05)
  )
  tmp <- tempfile(fileext = ".html")
  expect_message(
    spot_map(df, all_cases = TRUE, output = tmp),
    "Cases-only mode"
  )
  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("without all_cases, missing outcome column errors clearly", {
  df <- data.frame(
    fever = c("yes", "no"),
    lat   = c(11.0, 11.1),
    lon   = c(76.9, 77.0)
  )
  expect_error(
    spot_map(df, output = tempfile(fileext = ".html")),
    "Could not find outcome column"
  )
})

test_that("empty data frame errors clearly", {
  df <- data.frame(lat = numeric(0), lon = numeric(0),
                   case_control = character(0))
  expect_error(
    spot_map(df, output = tempfile(fileext = ".html")),
    "empty"
  )
})

test_that("rows with NA coordinates are dropped with a warning", {
  df <- data.frame(
    lat = c(11.0, NA, 11.2),
    lon = c(76.9, 77.0, 77.1),
    case_control = c("case", "case", "control")
  )
  tmp <- tempfile(fileext = ".html")
  expect_warning(
    spot_map(df, output = tmp),
    "missing or out-of-range"
  )
  unlink(tmp)
})

test_that("normal case/control data still builds", {
  df <- data.frame(
    lat = c(11.0, 11.1, 11.2),
    lon = c(76.9, 77.0, 77.1),
    case_control = c("case", "control", "case")
  )
  tmp <- tempfile(fileext = ".html")
  spot_map(df, output = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
})
