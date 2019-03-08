context("test-geometric timeline")

test_that("Geometric Timeline Label is created", {
  library(purrr)
  library(capstoneproject)
  library(dplyr)
  library(ggplot2)
  #library(lubridate);

  generateMapPlot <- function(){

      data(earthquakes)
      quakes_clean <- eq_clean_data(earthquakes)
      quakes_it_gr  <-  quakes_clean %>%
        dplyr::filter(COUNTRY %in% c('ITALY', 'GREECE') & DATE > '2000-01-01' & !is.na(EQ_PRIMARY))
      eq_map(quakes_it_gr, annot_col = 'DATE')
      }

  mapplot <- generateMapPlot()
  expect_true(all(class(mapplot) %in% c('leaflet','htmlwidget')))


})

