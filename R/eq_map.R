#' Visualize earthquakes on a map
#'
#' @param data A data.frame containing the earthquake data
#' @param annot_col A column name indicating the column to be used for annotation pop up
#'
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet addTiles addMarkers addProviderTiles
#'
#' @examples \dontrun{
#'   read_eq_clean_data() %>% eq_map()
#' }
#' @export
eq_map <- function(data, annot_col = "DATE") {
    leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)  %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = data, lng = ~ LONGITUDE, lat = ~ LATITUDE,
                                 radius = ~ EQ_PRIMARY, weight = 1)
}
