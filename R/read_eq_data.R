library(magrittr)

#' Read earthquake data on a clean dataframe
#'
#' @param raw_data_fn A path to the raw data filename, default to the signif.txt delivered by the
#'     package
#' @return A clean data.frame
#'
#' @importFrom readr read_delim
#'
#' @examples \dontrun{
#'   clean_data <- read_eq_clean_data()
#' }

#' @export
read_eq_clean_data <-function(raw_data_fn = file.path(system.file("extdata",
                                                                  package="CourseraRcapstone"),
                                                      "signif.txt")) {
    readr::read_delim(raw_data_fn, "\t") %>% eq_clean_data
}


#' Takes raw NOAA data frame and returns a clean data frame
#'
#' @param df The uncleaned data.frame
#' @return A clean data.frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_ coalesce
#' @importFrom tidyr unite_
#' @importFrom stringr str_to_title
#'
#' @details Date objects can only store positive years. BC years are marked with the IS_BC column
#' @examples \dontrun{
#'   raw_data <- readr::read_delim(raw_data_fn, "\t"))
#'   clean_data <- eq_clean_data(raw_data)
#' }
#' @export
eq_clean_data <-function(df) {
    df %>%
        dplyr::mutate_(YEARSTR = ~stringr::str_pad(as.character(abs(YEAR)), width = 4,
                                                   side = "left", pad = "0"),
                       DATESTR = ~paste(YEARSTR, MONTH, DAY, sep = "-"),
                       DATE = ~lubridate::ymd(DATESTR, truncated = 2),
                       LONGITUDE = ~as.numeric(LONGITUDE),
                       LATITUDE = ~as.numeric(LATITUDE)) %>%
        dplyr::mutate_(IS_BC = ~YEAR < 0) %>%
        eq_location_clean()
}

#' Clean up the LOCATION_NAME column
#' @param df The uncleaned data.frame
#' @return A clean data.frame
#'
#' @importFrom tidyr extract_
#' @importFrom dplyr mutate
#' @examples \dontrun{
#'   data <- uncleaned_data %>% eq_location_clean()
#' }
eq_location_clean <- function(df) {
    df %>%
        tidyr::extract_("LOCATION_NAME", c("LOCATION_NAME_COUNTRY", "LOCATION_NAME"),
                        regex = "(\\w+):\\s+(\\w.+)") %>%
        dplyr::mutate_(LOCATION_NAME = ~stringr::str_to_title(LOCATION_NAME))

}
