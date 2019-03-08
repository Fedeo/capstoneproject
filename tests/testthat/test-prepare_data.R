context("test-prepare_data")

test_that("Data are properly cleaned", {
  library(purrr)
  library(dplyr)

  data(earthquakes)
  quakes_clean <- eq_clean_data(earthquakes)

  expect_equal(as.character(quakes_clean$DATE[1]),"-2150-01-01")
  expect_equal(as.character(quakes_clean$DATE[14]),"-480-09-29")
  expect_equal(as.character(quakes_clean$DATE[1000]),"1678-10-02")
})

test_that("Location is cleaned", {
  library(purrr)
  library(dplyr)

  data(earthquakes)
  quakes_loc_clean <- eq_clean_location(earthquakes)

  expect_equal(quakes_loc_clean$LOCATION_NAME[1],"Bab-A-Daraa,Al-Karak")
})
