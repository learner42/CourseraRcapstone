#' Visualize earthquakes on a map
#'
#' @param data A data.frame containing the earthquake data
#' @param annot_col A column name indicating the column to be used for annotation pop up
#'
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet addTitles addMarkets
eq_map <- function(data, annot_col = "DATE") {
    leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = data, lng = ~ LONGITUDE, lat = ~ LATITUDE,
                                 radius = ~ EQ_PRIMARY, weight = 1)
}
