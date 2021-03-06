% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_data.R
\name{write_to_text}
\alias{write_to_text}
\title{Write data to text file}
\usage{
write_to_text(data, path)
}
\arguments{
\item{data}{List with data obtained using functions load_data_mean or load_data_elev}

\item{path}{Folder for storing the data}
}
\examples{
library(lubridate)
path_met <- '//hdata/grid/metdata/met_obs_v2.0'
path_runoff <- '//hdata/fou/Avrenningskart/Data/Runoff_All'
regine_main <- c('1.48','1.49','1.50')
time_vec <- seq(ymd("2011-01-01"), ymd("2011-01-04"), by = "days")
res <- load_data_mean(path_met, path_runoff, regine_main, time_vec)
path <- 'C:/Users/psan/Desktop/NVES_BESTA_DATA'
write_to_text(res, path)
}

