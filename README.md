# NVEDATA

R package for processing data at NVE, currently meteorological (SeNorge) and runoff data.

# Installation

In RStudio, install package devtools:

```R
install.packages("devtools")
```

Install the package with the following code:

```R
library(devtools)
install_github("jmgnve/NVEDATA")
```

# Example for accessing meta data

The following code returns metadata for all available stations:

```R
metadata <- get_metadata()
```

The metadata is stored in a data frame that can be used for selecting stations.

# Example for downloading data (catchment averaged) for a set of stations

```R
library(lubridate)
path_met <- '//hdata/grid/metdata/met_obs_v2.0'
path_runoff <- '//hdata/fou/Vannbalansekart/Data/Runoff_All'
regine_main <- c('1.48','1.49','1.50')
time_vec <- seq(ymd("2011-01-01"), ymd("2011-01-04"), by = "days")
res <- load_data_mean(path_met, path_runoff, regine_main, time_vec)
```
The meteorological data is averaged over the watershed.

# Example for downloading data (averages for elevation bands) for a set of stations

```R
library(lubridate)
path_met <- '//hdata/grid/metdata/met_obs_v2.0'
path_runoff <- '//hdata/fou/Vannbalansekart/Data/Runoff_All'
regine_main <- c('1.48','1.49','1.50')
time_vec <- seq(ymd("2011-01-01"), ymd("2011-01-04"), by = "days")
res <- load_data_elev(path_met, path_runoff, regine_main, time_vec)
```
The meteorological data is averaged using elevation bands (0 to 200m, 200 to 400m...).

# Example for writing the results to text files

```R
library(lubridate)
path_met <- '//hdata/grid/metdata/met_obs_v2.0'
path_runoff <- '//hdata/fou/Vannbalansekart/Data/Runoff_All'
regine_main <- c('1.48','1.49','1.50')
time_vec <- seq(ymd("2011-01-01"), ymd("2011-01-04"), by = "days")
res <- load_data_mean(path_met, path_runoff, regine_main, time_vec)
path <- 'C:/Users/psan/Desktop/NVES_BESTA_DATA'
write_to_text(res, path)
```



