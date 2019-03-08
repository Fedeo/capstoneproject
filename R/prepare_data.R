#' Clean Earthquake Data
#'
#' \code{eq_clean_data} takes the raw data frame of NOAA earthquake and cleans it
#'
#' This function takes the raw data frame of NOAA earthquake data and cleans it.
#' Data (tab delimited) is available for download at:
#' \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{Significant
#' EarthQuake Database}
#' The clean data frame have the following:
#' 1. A date column created by uniting the year, month, day and converting it to
#'    the Date class
#' 2. LATITUDE and LONGITUDE columns converted to numeric class
#'
#'
#' @param data A data frame of NOAA significant earthquake data
#'
#' @return A \code{tbl_df} with the same supplied data, but with
#'   \code{LATITUDE}, \code{LONGITUDE}, \code{DEATHS}, \code{TOTAL_DEATHS}, and
#'   \code{EQ_PRIMARY} converted from \code{character} to \code{numeric}, and a
#'   new column \code{DATE}.
#'
#' @author Federico Sensi
#'
#' @import  magrittr
#' @importFrom  dplyr mutate
#' @importFrom purrr pmap
#' @importFrom lubridate ymd years
#'
#' @export
#'
#' @examples
#'
#' data(earthquakes)
#' quakes_clean <- eq_clean_data(earthquakes)

eq_clean_data <- function(data) {

  data <- data %>%
  mutate(
      LONGITUDE = as.numeric(LONGITUDE),
      LATITUDE = as.numeric(LATITUDE),
      EQ_PRIMARY = as.numeric(EQ_PRIMARY),      #Not required but needed for geometry in lesson 2
      TOTAL_DEATHS =  as.numeric(TOTAL_DEATHS), #Not required but needed for geometry in lesson 2
      MONTH = ifelse(is.na(MONTH), 1, MONTH),
      DAY = ifelse(is.na(DAY), 1, DAY))

  data <- data %>%
      mutate(DATE = pmap(list(YEAR, MONTH, DAY),
                         function(y, m, d) {
                           #Handle BC dates with lubridate (see https://github.com/tidyverse/lubridate/issues/2)
                           if (y < 0) {

                             # get the abs of the year
                             abs_year <- abs(y)

                             # Mirror date with years 0000
                             mirror_date <- paste('0000',m,d)

                             #Subtract years to the mirror date
                             date <- ymd(mirror_date) - years(abs_year)

                           } else {
                             #Simple case
                             date <- as.Date(paste(d, m, y, sep = '-'), '%d-%m-%Y')
                           }

                           return(date)
                         })) %>%
    mutate(DATE = unlist(DATE),
                   DATE = as.Date(DATE, origin = '1970-01-01'))

  data
}

#' Clean Earthquake Location Field
#'
#'
#' This function cleans the \code{LOCATION_NAME} observation in a NOAA
#' Significant Earthquakes data set.  It removes the country name (of form
#' \code{COUNTRY:}) from the \code{LOCATION_NAME} data (unless the country is
#' the only name present in that field), and converts the remainder of the field
#' to Title Case. This will make the location name easier to read when plotting
#' and mapping.
#'
#' @param data A data frame of NOAA significant earthquake data.
#'
#' @return A \code{tbl_df} with the same supplied data, but with the
#'   \code{LOCATION_NAME} variable cleaned up to remove the country name that is
#'   supplied in the \code{COUNTRY} variable (unless the country name is the
#'   \strong{only} word in the \code{LOCATION_NAME} observation), and convert
#'   the remainder of the words in \code{LOCATION_NAME} to Title Case.
#'
#' @author Federico Sensi
#'
#' @import  magrittr
#' @importFrom dplyr mutate
#' @importFrom purrr map2_chr
#' @importFrom stringr str_trim str_to_title
#'
#' @export
#'
#' @examples
#' data(earthquakes)
#' quakes_loc_clean <- eq_clean_location(earthquakes)
#'
eq_clean_location <- function(data) {

  data <- data %>%
    mutate(
      LOCATION_NAME =
        map2_chr(COUNTRY, LOCATION_NAME,
                     function(COUNTRY, LOCATION_NAME) {
                       gsub(paste0(COUNTRY, ":"), '', LOCATION_NAME)
                     }),
      LOCATION_NAME = str_trim(LOCATION_NAME),
      LOCATION_NAME = str_to_title(LOCATION_NAME)
    )

  data
}

