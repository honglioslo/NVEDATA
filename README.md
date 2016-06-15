# NVEDATA

R package for processing data at NVE, currently only SeNorge data.

# Installation

On RStudio, install package devtools:

> install.packages("devtools")

Afterwards, run these commands:

> library(devtools)
> install_github("jmg/NVEDATA")

# Example

The following example downloads data for three stations over five days:

> library(NVEDATA)

> path = "//hdata/grid/metdata/met_obs_v2.0"

> station = c("1.48","1.49","1.50")

> time_vec = seq(as.Date("2011-01-01"), as.Date("2011-01-04"), by="days")

> res = load_data_mean(path,station,time_vec)



