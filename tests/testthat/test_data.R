test_that("eq_clean_data cleans the data", {
    raw_data <- data.frame(62,2,5,"40.7","14.5","ITALY: POMPEII")
    colnames(raw_data) <- c("YEAR", "MONTH", "DAY", "LONGITUDE", "LATITUDE", "LOCATION_NAME")
    data <- raw_data %>% eq_clean_data()
    clean_date = data$DATE[1]
    clean_location <- data$LOCATION_NAME[1]
    expect_equal(clean_location, "Pompeii")
    expect_equal(clean_date, lubridate::ymd("0062-02-05"))
    is_bc <- data$IS_BC[1]
    expect_false(is_bc)
})

test_that("eq_clean_data converts BC year", {
    raw_data <- data.frame(-464, "NA", "NA","37.08","22.43","GREECE: SPARTA")
    colnames(raw_data) <- c("YEAR", "MONTH", "DAY", "LONGITUDE", "LATITUDE", "LOCATION_NAME")
    data <- raw_data %>% eq_clean_data()
    clean_date <- data$DATE[1]
    is_bc <- data$IS_BC[1]
    expect_true(is_bc)
    expect_equal(clean_date, lubridate::ymd("0464-01-01"))
})


test_that("eq_location_clean cleans the location name", {
    raw_data <- data.frame(62,2,5,"40.7","14.5","ITALY: POMPEII")
    colnames(raw_data) <- c("YEAR", "MONTH", "DAY", "LONGITUDE", "LATITUDE", "LOCATION_NAME")
    data <- raw_data %>% eq_location_clean()
    clean_location <- data$LOCATION_NAME[1]
    expect_equal(clean_location, "Pompeii")
})

test_that("read_eq_clean_data reads the data frame from the package data", {
    data <- read_eq_clean_data()
    expect_true(is.data.frame(data))
})
