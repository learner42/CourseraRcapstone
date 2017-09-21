library(magrittr)

#' Read earthquake data on a clean dataframe
#'
#' @param raw_data_fn A path to the raw data filename, default to the signif.txt delivered by the
#'     package
#'
#' @return A clean data.frame
#' @export
read_eq_clean_data <-function(raw_data_fn = file.path(system.file("extdata",
                                                                  package="CourseraRcapstone"),
                                                      "signif.txt")) {
    readr::read_delim(raw_data_fn, "\t") %>% eq_clean_data
}


#' Takes raw NOAA data frame and returns a clean data frame
#'
#' @param df The uncleaned data.frame
#'
#' @return A clean data.frame
#' @export
eq_clean_data <-function(df) {
    df %>%
        tidyr::unite(date, rlang::sym("YEAR"), rlang::sym("MONTH"), rlang::sym("DAY")) %>%
        dplyr::mutate(date = lubridate::ymd(date)) %>%
        dplyr::mutate(LONGITUDE = as.numeric(rlang::sym("LONGITUDE")),
                      LATITUDE = as.numeric(rlang::sym("LATITUDE"))) %>%
        tidyr::extract(rlang::sym("LOCATION_NAME"), c("LOCATION_NAME_COUNTRY", "LOCATION_NAME"),
                       regex = "(\\w+):\\s+(\\w.+)") %>%
        dplyr::mutate(LOCATION_NAME = stringr::str_to_title(rlang::sym("LOCATION_NAME")))
}
