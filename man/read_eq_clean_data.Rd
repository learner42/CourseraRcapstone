% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_eq_data.R
\name{read_eq_clean_data}
\alias{read_eq_clean_data}
\title{Read earthquake data on a clean dataframe}
\usage{
read_eq_clean_data(raw_data_fn = file.path(system.file("extdata", package =
  "CourseraRcapstone"), "signif.txt"))
}
\arguments{
\item{raw_data_fn}{A path to the raw data filename, default to the signif.txt delivered by the
package}
}
\value{
A clean data.frame
}
\description{
Read earthquake data on a clean dataframe
}
\examples{
\dontrun{
  clean_data <- read_eq_clean_data()
}
}
