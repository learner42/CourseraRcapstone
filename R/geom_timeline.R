#' Custom Geom proto for the timeline
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob unit gpar
#'
#' @export
GeomTimeline <-
    ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                     required_aes = c("x"),
                     default_aes = ggplot2::aes(y = "NA", size = 5, colour = "grey", fill = "grey", alpha = 0.8, stroke = 0.5, shape = 1),
                     draw_key = ggplot2::draw_key_point,
                     draw_panel = function(data, panel_scales, coord) {

                         ## Use a constant if the optional aesthetic is not defined
                         if (!("y" %in% colnames(data))) {
                             data$y <- 0.1
                         }


                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)

                         ## Let's print out the structure of the 'coords' object
                         ## str(coords)

                         ## Construct a grid grob
                         grid::pointsGrob(
                             coords$x,
                             coords$y,
                             pch = coords$shape,
                             size = grid::unit(0.5*coords$size, "char"),
                             gp = grid::gpar(col = coords$colour,
                                             fill = coords$fill,
                                             alpha = coord$alpha)
                             )
                     })


#' Actual function that draw the earthquake timeline
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob unit gpar
#'
#' @inheritParams ggplot2::geom_point
#'
#' @importFrom ggplot2 layer
#'
#' @examples \dontrun{
#'     read_eq_clean_data() %>%
#'       dplyr::filter(YEAR > 2000 & !IS_BC & COUNTRY %in% c("USA", "CANADA")) %>%
#'       dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
#'                     EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
#'       ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                                     y = COUNTRY,
#'                                     color = TOTAL_DEATHS,
#'                                     fill = TOTAL_DEATHS,
#'                                     size = EQ_PRIMARY
#'                                     )) +
#'       geom_timeline()
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimeline, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
