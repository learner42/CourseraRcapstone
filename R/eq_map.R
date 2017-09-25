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

#' Create an HTML label for the leaflet map
#'
#' @param data The input data.frame
#' @return A vector of label strings
#'
#' @importFrom dplyr mutate
#'
#' @details This function should put together a character string for each earthquake that will show
#'     the cleaned location (as cleaned by the eq_location_clean() function created in Module 1),
#'     the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS), with boldface
#'     labels for each ("Location", "Total deaths", and "Magnitude"). If an earthquake is missing
#'     values for any of these, both the label and the value should be skipped for that element of
#'     the tag.
#'
#' @examples \dontrun{
#'   data %>%
#'     dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'     eq_map(annot_col = "popup_text")
#' }
#' @export
eq_create_label <- function(data) {
    line1 <- ifelse(is.na(data$LOCATION_NAME),
                    "",
                    paste("<b>Location:</b>", data$LOCATION_NAME, "<br />"))

    line2 <- ifelse(is.na(data$EQ_PRIMARY),
                    "",
                    paste("<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />"))
    line3 <- ifelse(is.na(data$TOTAL_DEATHS),
                    "",
                    paste("<b>Total deaths:</b>", data$TOTAL_DEATHS, "<br />"))
    paste(line1, line2, line3)
}


#' Leaflet example, this function is used in the R console, and is not exported
## map_example <- function() {
##     read_eq_clean_data() %>%
##         dplyr::filter_(YEAR > 2000 & !IS_BC & COUNTRY %in% c("MEXICO")) %>%
##         dplyr::mutate(popup_text = eq_create_label(.)) %>%
##         eq_map(annot_col = "popup_text")
## }
