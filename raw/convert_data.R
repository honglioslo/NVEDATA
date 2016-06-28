# Script for preparing package data

library(devtools)
library(RSAGA)
source("utils.R")

# Index of watersheds in bil-files

wsh_index <- read_index_file("SeNorge.txt")

# Metadata from database

meta_data <- read_metadata_file("Serier_Avrenningskart.csv")

# Only keep metadata for stations with index_senorge

meta_data <- filter(meta_data, drainage_basin_key %in% names(wsh_index))

# Process elevation data

dem_vec <- process_elevation_data("dtem1_nsfr.asc")

# Store for R package

devtools::use_data(wsh_index, meta_data, dem_vec, overwrite = TRUE, internal = FALSE)
