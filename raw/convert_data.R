# Script for preparing package data


library(devtools)
source("utils.R")

# Index of watersheds in bil-files

wsh_index <- read_index_file("SeNorge.txt")

# Metadata from database

meta_data <- read_metadata_file("Serier_Avrenningskart.csv")

# Store for R package

devtools::use_data(wsh_index, meta_data, overwrite = TRUE, internal = TRUE)
