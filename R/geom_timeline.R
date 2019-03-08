#' geom_timeline - adds an earthquake timeline plot to a ggplot2/grid graphics objects
#'
#' \code{geom_timeline} shows a timeline of NOAA Significant earthquakes,
#' plotting individual countries along the y-axis and dates along the x-axis.
#' The size of the points is relatative to the earthquakes' magnitude, and
#' the color is related to the total number of deaths.
#'
#'
#' @param mapping	Set of aesthetic mappings created by \code{\link{aes}} or \code{\link{aes_}}.
#'   If specified and \code{inherit.aes = TRUE} (the default), it is combined with the default mapping at the top level of the plot.
#'   Required aesthetics are: \code{x} (usually center longitude), \code{y} (usually center latitude), \code{r_ne}, \code{r_se}, \code{r_sw}, \code{r_nw}  (the northeast, southeast, southwest, and northwest wind radius measurements, respectively), \code{fill}, \code{color} (values to use for fill and color, usually wind speed for the measurements)
#'   Optional aesthetics are: \code{scale_radii} (optional scaling factor for the wind radii, default is \code{1.0})
#' @param data The data to be displayed in this layer. There are three options:
#'   If \code{NULL}, the default, the data is inherited from the plot data as specified in the call to \code{\link{ggplot}}.
#'   A \code{data.frame}, or other object, will override the plot data. All objects will be fortified to produce a data frame.
#'   See \code{\link{fortify}} for which variables will be created.
#'   A \code{function} will be called with a single argument, the plot data. The return value must be a \code{data.frame.},
#'   and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend	logical. Should this layer be included in the legends?
#' @param inherit.aes	If \code{FALSE}, overrides the default aesthetics, rather than combining with them.
#'
#' @return This function returns a \code{ggplot2::layer}.
#'
#' @author Federico Sensi
#'
#' @import  magrittr
#' @importFrom ggplot2 layer
#' @export
#'
#'
#' @examples
#'
#' library(magrittr)
#' data(earthquakes)
#' quakes_clean <- eq_clean_data(earthquakes)
#'
#'  quakes_clean %>%
#'     dplyr::filter(COUNTRY %in% c('ITALY', 'GREECE') & DATE > '1990-01-01' & !is.na(EQ_PRIMARY)) %>%
#'     ggplot() +
#'     geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
#'                       size = EQ_PRIMARY)) +
#'     scale_size_continuous(name = 'Richter scale value') +
#'     scale_color_continuous(name = 'Nbr of Deaths')

geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' GeomTimeline
#'
#' See \code{\link{geom_timeline}} for description.
#'
#' @import  magrittr
#' @importFrom ggplot2 draw_key_point ggproto aes Geom
#' @importFrom grid pointsGrob gpar
#'
#' @keywords internal

GeomTimeline <- ggproto("GeomTimeline", Geom,
                         required_aes = c('x'),

                         default_aes = aes(
                           y = 0,
                           size = 1,
                           color = 'grey50',
                           alpha = 0.5,
                           shape = 19,
                           stroke = 0.5,
                           fill = NA
                         ),

                         draw_key = draw_key_point,

                         draw_panel = function(data, panel_scales, coord) {

                           coords <- coord$transform(data, panel_scales)

                           # resize the coordinates' size to more reasonable values
                           coords$size <-
                             coords$size / max(coords$size) * 1.5

                           ## Construct a grid grob
                           pointsGrob(
                             x = coords$x,
                             y = coords$y,
                             pch = coords$shape,
                             gp = gpar(
                               col = coords$colour,
                               alpha = coords$alpha,
                               cex = coords$size
                             )
                           )
                    }
)
