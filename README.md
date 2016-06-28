# NVEDATA

R package for processing data at NVE, currently only SeNorge data.

# Installation

On RStudio, install package devtools:

```R
install.packages("devtools")
```

Afterwards, run these commands:

```R
library(devtools)
install_github("jmgnve/NVEDATA")
```

# Example

The following example downloads data for three stations:

```R
library(NVEDATA)
path = "//hdata/grid/metdata/met_obs_v2.0"
regine_main = c("1.48","1.49","1.50")
time_vec = seq(as.Date("2011-01-01"), as.Date("2011-01-04"), by="days")
res = load_data_mean(path,regine_main,time_vec)
```

The following example downloads metadata for three stations:

```R
regine_main = c("1.48","1.49","1.50")
metadata <- get_metadata(regine_main)
```


