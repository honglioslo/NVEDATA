#' @title Load metadata for a set of stations
#' @param Vector with stations (if not provided, the default returns all stations available)
#' @return A dataframe with metadata
#' @examples
#' regine_main <- c('1.48','1.49','1.50')
#' metadata <- get_metadata(regine_main)
#' @export

get_metadata <- function(regine_main = meta_data$regine_main) {

  isel <- sapply(regine_main, FUN = function(x) which(meta_data$regine_main == x))

  return(meta_data[isel, ])

}


#' @title Load meteorological data (air temperature and precipitation) for a set of stations
#' @param path Path to folder with BIL files (currently only works for SeNorge version 2.0 and 2.1)
#' @param regine_main Vector with stations
#' @param time_vec Vector with dates
#' @examples
#' path <- '//hdata/grid/metdata/met_obs_v2.0'
#' regine_main <- c('1.48','1.49','1.50')
#' time_vec <- seq(as.Date('2011-01-01'), as.Date('2011-01-04'), by='days')
#' res <- load_data_mean(path,regine_main,time_vec)
#' @return A list with data for each station
#' @export

load_data_mean <- function(path, regine_main, time_vec) {

  # Test if metadata exist for stations

  if (!all(regine_main %in% meta_data$regine_main)) {

    imissing <- !(regine_main %in% meta_data$regine_main)

    station_str <- paste(regine_main[imissing], collapse = ", ")

    stop(paste("Lacking metadata for station", station_str, sep = " "))

  }

  # Test if wsh_index exists for stations

  stats_missing <- meta_data$regine_main[!meta_data$drainage_basin_key %in% names(wsh_index)]

  if (any(regine_main %in% stats_missing)) {

    imissing <- regine_main %in% stats_missing

    station_str <- paste(regine_main[imissing], collapse = ", ")

    stop(paste("Lacking wsh_index for station", station_str, sep = " "))

  }

  # Initilize list for one station

  init_list <- function(istat) {

    data_all[[istat]] <- list(time_vec = time_vec, regine_main = regine_main[istat], Tair = NA, Prec = NA)

  }

  # Assign variable to general data structure

  assign_var <- function(istat, iday, variable) {

    data_all[[istat]][[variable]][iday] <- met_data[[istat]]
    data_all[[istat]]

  }

  # Initlize data structure

  nstations <- length(regine_main)

  data_all <- vector("list", nstations)

  data_all <- lapply(seq_along(data_all), init_list)

  # Loop over time

  for (iday in seq_along(time_vec)) {


    print(paste("Processing day ", iday, sep = ""))


    # Process air temperature data

    filename <- paste(path, "/tm/", format(time_vec[iday], "%Y"), "/tm_", format(time_vec[iday], "%Y_%m_%d"), ".nc", sep = "")

    met_data <- read_nc_file(filename)

    # Extract data for stations

    met_data <- lapply(regine_main, load_single_wsh, grid_data = met_data)

    # Average data

    met_data <- lapply(met_data, mean)

    # Assign variable to data structure

    data_all <- lapply(seq_along(data_all), assign_var, iday = iday, variable = "Tair")


    # Process precipitation data

    filename <- paste(path, "/rr/", format(time_vec[iday], "%Y"), "/rr_", format(time_vec[iday], "%Y_%m_%d"), ".nc", sep = "")

    met_data <- read_nc_file(filename)

    # Extract data for stations

    met_data <- lapply(regine_main, load_single_wsh, grid_data = met_data)

    # Average data

    met_data <- lapply(met_data, mean)

    # Assign variable to data structure

    data_all <- lapply(seq_along(data_all), assign_var, iday = iday, variable = "Prec")

  }

  return(data_all)

}






