#' Label Largest Earthquakes on Timeline
#'
#' \code{geom_timeline_label} works best when used with
#' \code{\link{geom_timeline}}, labeling the top \code{n} earthquakes, by
#' magnitude, with a specified label field.  By default, the labels are for
#' the top 5 earthquakes for each country specified, however, the user may
#' adjust this with the \code{n_max} aesthetic.
#'
#' @param mapping See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param data See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param stat See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param position See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param na.rm See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param show.legend See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param inherit.aes See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param ... other arguments passed on to \code{\link{layer}}.
#'
#' @section Aesthetics:
#' \code{geom_timeline_label} undertands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'  \item \strong{x}: recommend \code{DATE}
#'  \item \strong{label}: recommend \code{LOCATION_NAME}
#'  \item \strong{magnitude}: recommend \code{EQ_PRIMARY}
#'  \item y: recommend \code{COUNTRY}
#'  \item n_max: default 3. Top \code{n} earthquakes to label,
#'        sorted by magnitude.
#'  \item color
#'  \item linetype
#'  \item size
#'  \item alpha
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
#'
#' @examples
#' data(earthquakes)
#' quakes_clean <- eq_clean_data(earthquakes)
#'
#'  quakes_clean %>%
#'     dplyr::filter(COUNTRY %in% c('ITALY', 'GREECE') & DATE > '2000-01-01' & !is.na(EQ_PRIMARY)) %>%
#'     ggplot() +
#'     geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
#'                       size = EQ_PRIMARY)) +
#'     geom_timeline_label(aes(x = DATE, y = COUNTRY, magnitude = EQ_PRIMARY,
#'                       label = LOCATION_NAME, n_max = 3)) +
#'     scale_size_continuous(name = 'Richter scale value') +
#'     scale_color_continuous(name = 'Nbr of Deaths')

geom_timeline_label <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    layer(
      geom = GeomTimelineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

#' GeomTimelineLabel
#'
#' See \code{\link{geom_timeline_label}} for description.
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point GeomSegment GeomText
#' @importFrom dplyr mutate group_by top_n
#' @importFrom grid grobTree
#'
#' @keywords internal
#'

GeomTimelineLabel <-
  ggproto(
    "GeomTimelineLabel",
    Geom,
    required_aes = c('x', 'label', 'magnitude'),

    default_aes = aes(
      n_max = 3,
      y = 0,
      color = 'grey50',
      size = 0.5,
      linetype = 1,
      alpha = NA
    ),

    draw_key = draw_key_point,

    draw_panel = function(data, panel_scales, coord) {

      n_max <-data$n_max[1]

      # get to n earthquakes by magnitude
      data <- data %>%
        mutate(magnitude = magnitude / max(magnitude) * 1.5) %>%
        group_by(group) %>%
        top_n(n_max, magnitude)

      # grob for vertical line
      data$xend <- data$x
      data$yend <- data$y + 0.1
      g1 <- GeomSegment$draw_panel(unique(data), panel_scales, coord)

      # grob for text label
      data$y <- data$yend + 0.03
      data$angle <- 45
      data$fontface <- 9
      data$lineheight <- 2
      data$hjust <- 'left'
      data$vjust <- 'top'
      data$family <- 'sans'
      data$size <- 3
      data$colour <- 'black'
      g2 <- GeomText$draw_panel(unique(data), panel_scales, coord)

      ggplot2:::ggname('geom_timeline_label', grobTree(g1, g2))
    }
  )
