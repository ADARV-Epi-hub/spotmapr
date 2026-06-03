test_that("detect_lat_lon finds named columns", {
  df <- data.frame(latitude = c(28.6, 19.0), longitude = c(77.2, 72.8),
                   outcome = c("case", "control"))
  result <- detect_lat_lon(df)
  expect_equal(result$lat, "latitude")
  expect_equal(result$lon, "longitude")
})

test_that("detect_lat_lon with explicit columns", {
  df <- data.frame(y = c(28.6, 19.0), x = c(77.2, 72.8))
  result <- detect_lat_lon(df, lat_col = "y", lon_col = "x")
  expect_equal(result$lat, "y")
  expect_equal(result$lon, "x")
})

test_that("detect_lat_lon errors on missing explicit columns", {
  df <- data.frame(a = 1, b = 2)
  expect_error(detect_lat_lon(df, lat_col = "lat", lon_col = "lon"),
               "not found")
})

test_that("detect_outcome finds standard column names", {
  df <- data.frame(lat = 1, lon = 2, outcome = c("case", "control", "case"))
  result <- detect_outcome(df)
  expect_equal(result$outcome_col, "outcome")
  expect_equal(result$case_value, "case")
})

test_that("detect_outcome uses explicit values", {
  df <- data.frame(status = c("pos", "neg", "pos"))
  result <- detect_outcome(df, outcome_col = "status", case_value = "pos")
  expect_equal(result$outcome_col, "status")
  expect_equal(result$case_value, "pos")
})

test_that("detect_outcome errors on missing column", {
  df <- data.frame(a = 1)
  expect_error(detect_outcome(df, outcome_col = "missing"), "not found")
})

test_that("detect_outcome errors when case_value not in column", {
  df <- data.frame(status = c("foo", "bar", "foo"))
  expect_error(
    detect_outcome(df, outcome_col = "status", case_value = "case"),
    "not found in column"
  )
})

test_that("detect_lat_lon returns df in result list", {
  df <- data.frame(lat = c(28.6, 19.0), long = c(77.2, 72.8))
  result <- detect_lat_lon(df)
  expect_true("df" %in% names(result))
  expect_s3_class(result$df, "data.frame")
})

test_that("detect_lat_lon splits a combined lat,lon column", {
  df <- data.frame(coords = c("28.6, 77.2", "19.0, 72.8"))
  result <- detect_lat_lon(df)
  expect_equal(result$lat, ".spotmapr_auto_lat")
  expect_equal(result$lon, ".spotmapr_auto_lon")
  expect_true(".spotmapr_auto_lat" %in% names(result$df))
  expect_true(".spotmapr_auto_lon" %in% names(result$df))
  expect_equal(result$df[[".spotmapr_auto_lat"]], c(28.6, 19.0))
  expect_equal(result$df[[".spotmapr_auto_lon"]], c(77.2, 72.8))
})

test_that("detect_lat_lon accepts scientific-notation coordinates", {
  df <- data.frame(
    lat = c("1.1e1", "1.11e1", "1.12e1"),
    lon = c("7.69e1", "7.70e1", "7.71e1")
  )
  result <- detect_lat_lon(df)
  expect_equal(result$lat, "lat")
  expect_equal(result$lon, "lon")
})
