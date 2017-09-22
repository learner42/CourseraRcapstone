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
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr unite_ extract_
#' @importFrom stringr str_to_title
#'
#' @examples \dontrun{
#'   raw_data <- readr::read_delim(raw_data_fn, "\t"))
#'   clean_data <- eq_clean_data(raw_data)
#' }
#' @export
eq_clean_data <-function(df) {
    df %>%
        tidyr::unite_("DATE", c("YEAR", "MONTH", "DAY")) %>%
        dplyr::mutate_(DATE = ~lubridate::ymd(DATE)) %>%
        dplyr::mutate_(LONGITUDE = ~as.numeric(LONGITUDE),
                       LATITUDE = ~as.numeric(LATITUDE)) %>%
        eq_location_clean()
}

#' Clean up the LOCATION_NAME column
#' @param df The uncleaned data.frame
#' @return A clean data.frame
#'
#' @importFrom tidyr unite_ extract_
#' @importFrom dplyr mutate_
eq_location_clean <- function(df) {
    df %>%
        tidyr::extract_("LOCATION_NAME", c("LOCATION_NAME_COUNTRY", "LOCATION_NAME"),
                        regex = "(\\w+):\\s+(\\w.+)") %>%
        dplyr::mutate_(LOCATION_NAME = ~stringr::str_to_title(LOCATION_NAME))

}
