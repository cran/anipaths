# process data
vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
vultures_paths <- vultures[vultures$POSIX > as.POSIXct("2009-03-01", origin = "1970-01-01") &
  vultures$POSIX < as.POSIXct("2009-05-01", origin = "1970-01-01"), ]

test_that("return animation paths after interpolation using GAM", {
  # run GAM interpolation
  gam_int_results <- animate_paths(
    paths = vultures_paths,
    delta.t = "week",
    coord = c("location.long", "location.lat"),
    Time.name = "POSIX",
    ID.name = "individual.local.identifier",
    return.paths = TRUE
  )
  expect_length(gam_int_results, 9)
  expect_equal(length(gam_int_results$paths), length(gam_int_results$paths.interp))
})

test_that("error when generating animation with Google map background", {
  background <- list(
    center = c(-90, 10),
    zoom = 3,
    maptype = "satellite"
  )
  expect_error(animate_paths(
    paths = vultures_paths,
    delta.t = "week",
    coord = c("location.long", "location.lat"),
    Time.name = "POSIX",
    ID.name = "individual.local.identifier",
    background = background
  ), paste(
    "Google maps now requires an API key. Once you have registered",
    "an account with Google here (https://cloud.google.com/maps-platform/),",
    "you can provide the API key via the ggmap function",
    "register_google(key = 'YOUR_API_KEY')."
  ), fixed = TRUE)
})
