context("test-geometric timeline")

test_that("Geomteric Timeline is working properly", {
  library(purrr)
  library(capstoneproject)
  library(dplyr)
  library(ggplot2)

  generatePlot <- function(){
      data(earthquakes)
      quakes_clean <- eq_clean_data(earthquakes)

      output <- data.frame()
      quakes_clean %>%
        filter(COUNTRY %in% c('ITALY', 'GREECE') & DATE > '1990-01-01' & !is.na(EQ_PRIMARY)) %>%
        ggplot() +
        geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                          size = EQ_PRIMARY)) +
        scale_size_continuous(name = 'Richter scale value') +
        scale_color_continuous(name = 'Nbr of Deaths')
      }

  geomplot <- generatePlot()
  expect_true(all(class(geomplot$layers[[1]]$geom) %in% c('GeomTimeline','ggproto','Geom','gg')))


})

