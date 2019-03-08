context("test-geometric timeline")

test_that("Geometric Timeline Label is created", {
  library(purrr)
  library(capstoneproject)
  library(dplyr)
  library(ggplot2)

  generatePlot <- function(){

      data(earthquakes)
      quakes_clean <- eq_clean_data(earthquakes)

      quakes_clean %>%
        dplyr::filter(COUNTRY %in% c('ITALY', 'GREECE') & DATE > '2000-01-01' & !is.na(EQ_PRIMARY)) %>%
        ggplot() +
        geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                          size = EQ_PRIMARY)) +
        geom_timeline_label(aes(x = DATE, y = COUNTRY, magnitude = EQ_PRIMARY,
                                label = LOCATION_NAME, n_max = 3)) +
        scale_size_continuous(name = 'Richter scale value') +
        scale_color_continuous(name = 'Nbr of Deaths')
      }

  geomplot <- generatePlot()
  expect_true(all(class(geomplot$layers[[1]]$geom) %in% c('GeomTimeline','ggproto','Geom','gg')))


})

