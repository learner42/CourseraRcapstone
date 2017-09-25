#' Actual function that draw the earthquake timeline
#'
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 layer
#'
#' @details In a typical use case, the user should specify the DATE as the x aesthetic, a grouping,
#'     e.g country for y, and 2 other measurements for color and size, e.g. TOTAL_DEATHS and
#'     EQ_PRIMARY (earthquake maginitude).
#' @examples \dontrun{
#'     read_eq_clean_data() %>%
#'       dplyr::filter(YEAR > 2000 & !IS_BC & COUNTRY %in% c("USA", "CANADA")) %>%
#'       dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
#'                     EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
#'       ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                                    y = COUNTRY,
#'                                    color = TOTAL_DEATHS,
#'                                    size = EQ_PRIMARY
#'                                    )) +
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

#' Actual function that draw the earthquake timeline label
#'
#'
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 layer
#' @param n_max A integer describing the maximum numer of labels to display
#' @details In a typical use case, the x aesthetic is already inherited from the previous layer, the
#'     user only have to specify the dimension to be used as label, as well as the maximum number of
#'     labels to be displayed
#'
#' @examples \dontrun{
#'     read_eq_clean_data() %>%
#'         dplyr::filter(YEAR > 2000 & !IS_BC & COUNTRY %in% c("USA", "CANADA")) %>%
#'         dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
#'                       EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
#'         ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                                      y = COUNTRY,
#'                                      colour = TOTAL_DEATHS,
#'                                      size = EQ_PRIMARY
#'                                      )) +
#'         geom_timeline() +
#'         theme_timeline() +
#'         labs(size = "Richter scale value", color = "# deaths") +
    ##     geom_timeline_label(ggplot2::aes(label = LOCATION_NAME), n_max = 3)
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, n_max = NULL,
                          show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimelineLabel, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n_max = n_max, ...)
    )
}


#' Specific them for our timelines
#' @return The theme for timeline goems
#' @export
theme_timeline <- function() {
    ggplot2::theme_bw() +
    ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        legend.position = "bottom"
    )
}

#' Custom Geom proto for timeline labesl
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#'
#' @details This proto is not exported
GeomTimelineLabel <-
    ggplot2::ggproto(
        "GeomTimelineLabel", ggplot2::Geom,
        required_aes = c("x", "label"),
        draw_key = ggplot2::draw_key_blank,
        draw_panel = function(data, panel_scales, coord, n_max) {
            if (!is.null(n_max)) {
                data <- data %>%
                    dplyr::group_by_("group") %>%
                    dplyr::top_n(n_max, size) %>%
                    dplyr::ungroup()
            }

            if (!("y" %in% colnames(data))) {
                data$y <- 0.1
            }

            coords <- coord$transform(data, panel_scales)
            group_count <- length(unique(data$group))
            offset <- 0.1 / group_count

            lines <- grid::polylineGrob(
                x = unit(c(coords$x, coords$x), "npc"),
                y = unit(c(coords$y, coords$y + offset), "npc"),
                id = rep(1:dim(coords)[1], 2),
                gp = grid::gpar(
                    col = "grey"
                )
            )

            names <- grid::textGrob(
                label = coords$label,
                x = unit(coords$x, "npc"),
                y = unit(coords$y + offset, "npc"),
                just = c("left", "bottom"),
                rot = 30
            )

            grid::gList(lines, names)
        }
    )


#' Custom Geom proto for the timeline
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob unit gpar polylineGrob addGrob
#' @details This proto is not exported
GeomTimeline <-
    ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                     required_aes = c("x"),
                     default_aes = ggplot2::aes(y = "NA", size = 5, colour = "grey", fill = "grey", alpha = 0.3, stroke = 0.5, shape = 21),
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
                         tree = grid::gTree()
                         tree <- grid::addGrob(
                             tree,
                             grid::pointsGrob(
                                 coords$x,
                                 coords$y,
                                 pch = coords$shape,
                                 size = grid::unit(0.5*coords$size, "char"),
                                 gp = grid::gpar(col = coords$colour,
                                                 fill = coords$colour,
                                                 alpha = coords$alpha)
                             )
                         )
                         ys = unique(coords$y)
                         xs = c(min(coords$x), max(coords$x))

                         tree <- grid::addGrob(
                             tree,
                             grid::polylineGrob(
                                 x = unit(rep(xs, each = length(ys)), "npc"),
                                 y = unit(c(ys, ys), "npc"),
                                 id = rep(seq_along(ys), 2),
                                 gp = grid::gpar(col = "grey",
                                                 lwd = .pt)
                             )
                             )
                         tree
                     })
