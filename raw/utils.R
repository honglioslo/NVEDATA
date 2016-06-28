

library(dplyr)
library(RSAGA)


###############################

read_index_file <- function(filename) {

  fid <- file(filename)

  data <- readLines(fid)

  close(fid)

  data <- strsplit(data, split = " ")

  wsh_index <- vector("list", length(data))
  drainage_basin_key <- vector("integer", length(data))

  for (iwsh in 1:length(data)) {

    wsh_index[[iwsh]] <- as.integer(data[[iwsh]][5:length(data[[iwsh]])])

    drainage_basin_key[iwsh] <- data[[iwsh]][4]

  }

  names(wsh_index) <- drainage_basin_key

  return(wsh_index)

}


###############################

read_metadata_file <- function(filename) {

  # Read station metadata

  meta_data <- read.csv(filename, header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

  meta_data <- tbl_df(meta_data)

  # Keep rows with runoff data (parameter == 1001)

  meta_data <- filter(meta_data, param_key==1001)

  # Remove duplicated stations

  idup <- duplicated(meta_data[, 1:3])

  meta_data <- meta_data[!idup, ]

  # Add station name as 'regine_area.main_no'

  meta_data <- mutate(meta_data, regine_main = paste(regine_area, main_no, sep = "."))

  # Add observation series as 'regine_area.main_no.point_no.param_key.version_no_end'

  meta_data <- mutate(meta_data, obs_series = paste(regine_area, main_no, point_no, param_key, version_no_end, sep = "."))

  return(meta_data)

}

###############################

process_elevation_data <- function(filename) {

  # Read and flip grid to match orientation of senorge data

  dtm <- read.ascii.grid("dtem1_nsfr.asc", return.header = FALSE)

  # Tranpose to save by rows

  dtm <- t(dtm)

  # Convert to vector

  dtm <- as.vector(dtm)

  return(dtm)

}





