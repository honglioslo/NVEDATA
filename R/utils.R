


# #' @title Load runoff data from lescon_var output files
# #' @param obs_series File name in format "regine_area.main_no.point_no.param_key.version_end_no"
# #' @param path Path to folder with files
# #' @param time_vec Vector with times defining the period for data selection
# #' @export
#
# read_runoff_file <- function(data, path, time_vec) {
#
#   # Load file
#
#   filename <- paste(path, "/", data$metadata$obs_series, sep = "")
#
#   qdata <- read.table(filename, col.names = c("Time", "Value"), stringsAsFactors = FALSE)
#
#   qdata <- dplyr::tbl_df(qdata)
#
#   # Remove awkward rows
#
#   if (length(grep("prev", qdata$Time)) > 0) {
#
#     qdata <- qdata[-grep("prev", qdata$Time), ]
#
#   }
#
#   # Change format
#
#   qdata <- dplyr::mutate(qdata, Time = lubridate::ymd(Time), Value = as.numeric(Value))
#
#   qdata$Value[qdata$Value == -9999] <- NA
#
#   # Select time period
#
#   istart <- which(qdata$Time == head(time_vec, 1))
#
#   istop <- which(qdata$Time == tail(time_vec, 1))
#
#   # Get runoff series
#
#   data$Runoff <- qdata$Value[istart:istop]
#
#   return(data)
#
# }


#' @title Load runoff data from lescon_var output files
#' @param path Path to folder with files
#' @param filename File name in format "regine_area.main_no.point_no.param_key.version_end_no"
#' @param time_vec Vector with times defining the period for data selection
#' @return Data frame (dplyr) with time and runoff
#' @import dplyr
#' @import lubridate
#' @export

read_runoff_file <- function(path, filename) {

  # Load file

  filename <- paste(path, "/", filename, sep = "")

  # Load file

  qdata <- read.table(filename, col.names = c("Time", "Value"), stringsAsFactors = FALSE)

  qdata <- dplyr::tbl_df(qdata)

  # Remove awkward rows

  if (length(grep("prev", qdata$Time)) > 0) {

    qdata <- qdata[-grep("prev", qdata$Time), ]

  }

  # Change format

  qdata <- dplyr::mutate(qdata, Time = lubridate::ymd(Time), Value = as.numeric(Value))

  qdata$Value[qdata$Value == -9999] <- NA

  return(qdata)

}


#' @title Internal function for loading runoff data

load_runoff_all <-  function(data, path, time_vec) {

  # Load file

  qdata <- read_runoff_file(path,data$metadata$obs_series)

  # Convert from m3/s to mm/day

  qdata$Value <- (qdata$Value * 86400 * 1000) / (data$metadata$area_total * 1e6)

  # Select time period

  istart <- which(qdata$Time == head(time_vec, 1))

  istop <- which(qdata$Time == tail(time_vec, 1))

  # Get runoff series

  data$Runoff <- qdata$Value[istart:istop]

  return(data)

}


#' @title Read netdcf file
#' @param filename Name of the netcdf file
#' @return Vector with values (reshaped into a vector from a grid)
#' @import ncdf4
#' @export

read_nc_file <- function(filename) {

  fnc <- ncdf4::nc_open(filename)

  v1 <- fnc$var[[1]]

  data_nc <- ncdf4::ncvar_get(fnc,v1)

  # To get correct matrix orientation, transpose data_nc
  # We need to save the data by rows, therefore transpose again
  # Two transposes is no transpose ...

  data_nc <- as.vector(data_nc)

  ncdf4::nc_close(fnc)

  return(data_nc)

}


#' @title Read bil file
#' @param filename Name of the file in bil format
#' @param nrow Number of rows in grid file (SeNorge grids used as default)
#' @param ncol Number of columns in grid file (SeNorge grids used as default)
#' @return Vector with values (not reshaped into a grid)
#' @export

read_bil_file <- function(filename, nrow = 1550, ncol = 1195) {

  fid <- file(filename, "rb")

  grid_data <- readBin(fid, n = nrow * ncol, what = integer(), size = 2, signed = FALSE, endian = "little")

  close(fid)

  grid_data[grid_data == 65535] <- NA

  return(grid_data)

}


#' @title Extract data for a single watershed from gridded meteorological data
#' @param regine_main Station name (regine_area and main_no seperated by a full stop)
#' @param grid_data Meteorological data stored as a vector (see functions read_bil_file and read_nc_file for loading such data)
#' @return Vector of values
#' @export

load_single_wsh <- function(regine_main, grid_data) {

  irow <- which(meta_data$regine_main == regine_main)

  drainage_basin_key <- as.character(meta_data$drainage_basin_key[irow])

  return(grid_data[wsh_index[[drainage_basin_key]]])

}






# #' Look also at http://nve-wiki.nve.no/index.php/Web_Chart_Service#da_.28DATA_ARCHIVE.29
# #' @title Load runoff data from xgeo
# #' @param obs_series File name in format "regine_area.main_no.point_no.param_key.version_end_no"
# #' @param path Path to folder with files
# #' @param time_vec Vector with times defining the period for data selection
# #' @export
#
#
# read_runoff_xgeo <- function(data) {
#
#   # Start time
#
#   start_time <- format(head(data$time_vec,1), "%Y%m%dT0000")
#
#   # Stop time
#
#   stop_time <- format(tail(data$time_vec,1) + lubridate::days(1), "%Y%m%dT0000")
#
#   # Load data from xgeo interface
#
#   version <- 0
#
#   da <- 29  # Data archive
#
#   xgeo_url <-paste("http://h-web01.nve.no/chartserver/ShowData.aspx?req=getchart&ver=1.0&",
#                    "time=",start_time,";",stop_time,
#                    "&chd=ds=htsr,da=",da,",id=",data$regine_main,".0.1001.",version,",rt=1&vfmt=xml",sep="")
#
#   xmldata <- XML::xmlParse(xgeo_url)
#
#   tmp <- XML::xmlToDataFrame(XML::getNodeSet(xmldata,"//Point"),colClasses = c("character","double"))
#
#   if (length(data$time_vec) == nrow(tmp) ) {
#     data$Runoff <- as.numeric(tmp$Value)
#   } else {
#     data$Runoff <- rep(NA,length(data$time_vec))
#   }
#
#   return(data)
#
# }


##-----
# HACK FLO

#' @title Read HBV modelling results
#' @param filename Full path and name of the file with HBV modelling results ("demodata/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt" as default))
#' @return A dataframe with the modelling results and parameters
#' @import dplyr
#' @export

read_HBV_data <- function(filename = system.file("demodata/usikkerhet_grd/utskrift", "vfpost_usikkerhet.txt", package = "NVEDATA")) {

  # Get the regine numbers related to the station names in the HBV output file
  # Read it from the package for use anywhere
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  station_ref_name <- station_ref$V5

#   path <- '../Flood_forecasting/data/usikkerhet_grd/utskrift'  # /ut_test for HBV50
#   filename <- "vfpost_usikkerhet.txt"
#   filename <- paste(path, "/", filename, sep = "")

  file_connect <- file(filename, open = "rt")

  regine_main <- c()
  station_name <- c()
  time_vec <- c()
  precip <- c()
  temperature <- c()
  snow_storage <- c()
  modelled_raw <- c()
  modelled_corr <- c()
  modelled_H90 <- c()
  modelled_L90 <- c()
  modelled_H50 <- c()
  modelled_L50 <- c()
  measured <- c()

# skip 1 line
  readLines(file_connect, n = 1)

  x <- TRUE
  i = 0
  # read station name
  name <- substring(readLines(file_connect, n = 1), 7)
  index <- which(station_ref_name == name)

  if (length(index) >= 1) {
    regine <- regine_ref_nb[index]
  } else {
    regine <- "NA"
  }



  while (x == TRUE) {
    # skip 3 lines
    readLines(file_connect, n = 3)
    # get indices
    j <- i * 30
    k <- j + 21
    l <- k + 9

    station_name[(j+1): l] <- rep(name, 30)
    regine_main[(j+1): l] <- rep(regine, 30)

    ## PAST
    temp <- read.table(file_connect, nrows = 21)
    # Time appears as DD/MM-YYYY
    year <- substring(temp[,1], 7, 10)
    month <- substring(temp[,1], 4, 5)
    day <- substring(temp[,1], 1, 2)
    time_vec[(j+1):k] <- paste(year, "-", month, "-", day, sep = "")
    precip[(j+1):k] <- temp[, 2]
    temperature[(j+1):k] <- temp[, 3]
    snow_storage[(j+1):k] <- temp[, 4]
    modelled_raw[(j+1):k] <- temp[, 5]  # This is the simulated discharge during the past period without data assimilation
    modelled_corr[(j+1):k] <- rep(NA, 21)
    modelled_H90[(j+1):k] <- rep(NA, 21)
    modelled_L90[(j+1):k] <- rep(NA, 21)
    modelled_H50[(j+1):k] <- rep(NA, 21)
    modelled_L50[(j+1):k] <- rep(NA, 21)
    measured[(j+1):k] <- temp[, 6]

    # Skip the line that separate past and forecast
    readLines(file_connect, n = 1)
    ## FUTURE
    temp <- read.table(file_connect, nrows = 9)
    year <- substring(temp[,1], 7, 10)
    month <- substring(temp[,1], 4, 5)
    day <- substring(temp[,1], 1, 2)
    time_vec[(k+1):l] <- paste(year, "-", month, "-", day, sep = "")
    precip[(k+1):l] <- temp[, 2]
    temperature[(k+1):l] <- temp[, 3]
    snow_storage[(k+1):l] <- temp[, 4]
    modelled_raw[(k+1):l] <- temp[, 5]  # This is the simulated discharge during the forecast period without data assimilation
    modelled_corr[(k+1):l] <- temp[, 6] # This is the modelled forecast after correction by observed data
    modelled_H90[(k+1):l] <- temp[, 7]
    modelled_L90[(k+1):l] <- temp[, 8]
    modelled_H50[(k+1):l] <- temp[, 9]
    modelled_L50[(k+1):l] <- temp[, 10]
    measured[(k+1):l] <- rep(NA, 9)

    # skip 2 lines before next station
    readLines(file_connect, n = 2)

    station_line <- readLines(file_connect, n = 1)
    x <- grepl("Felt", station_line)

    # read station name
    name <- substring(station_line, 7)
    index <- which(station_ref_name == name)

    if (length(index) >= 1) {
      regine <- regine_ref_nb[index]
      # regine <- metadata$regine_main[index]
    } else {
      regine <- "NA"
    }

    # Break it we reach the end of the file
    if (length(x) == 0) {break}
    # current_line_old <- current_line
    i <- i + 1
  }

  HBV <- data.frame(regine.main = regine_main,
                    station.name = station_name,
                    nbname = paste(regine_main, "-", station_name, sep = ""),
                    time = time_vec,
                    Input_Precip = precip,
                    Input_Temp = temperature,
                    State_Snow = snow_storage,
                    Runoff_SimRaw = modelled_raw,
                    Runoff_SimCorr = modelled_corr,
                    Runoff_SimH90 = modelled_H90,
                    Runoff_SimL90 = modelled_L90,
                    Runoff_SimH50 = modelled_H50,
                    Runoff_SimL50 = modelled_L50,
                    Runoff_Obs = measured)
  HBV[HBV == -10000.000] <- NA
  HBV[HBV == -9999.000] <- NA
  HBV <- tbl_df(HBV)
  invisible(HBV)

}


#' @title Read HBV modelling results for -+ 50 percent precipitation
#' @param filename Full path and name of the file with HBV_2016 +-50% modelling results ("demodata/usikkerhet_grd/ut_test/vfp3030.txt" as default))
#' @return A dataframe with the modelling results and parameters
#' @import dplyr
#' @export

read_HBV_P <- function(filename = system.file("demodata/usikkerhet_grd/ut_test", "vfp3030.txt", package = "NVEDATA")) {

  # Get the regine numbers related to the station names in the HBV output file
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  station_ref_name <- station_ref$V5

  #   path <- '../Flood_forecasting/data/usikkerhet_grd/utskrift'  # /ut_test for HBV50
  #   filename <- "vfpost_usikkerhet.txt"
  #   filename <- paste(path, "/", filename, sep = "")

  file_connect <- file(filename, open = "rt")

  regine_main <- c()
  station_name <- c()
  time_vec <- c()
  precip <- c()
  temperature <- c()
  snow_storage <- c()
  modelled_raw <- c()
  modelled_corr <- c()
  modelled_M50 <- c()
  modelled_P50 <- c()
  measured <- c()

  # skip 1 line
  readLines(file_connect, n = 1)

  x <- TRUE
  i = 0
  # read station name
  name <- substring(readLines(file_connect, n = 1), 7)
  index <- which(station_ref_name == name)

  if (length(index) >= 1) {
    regine <- regine_ref_nb[index]
  } else {
    regine <- "NA"
  }



  while (x == TRUE) {
    # skip 3 lines
    readLines(file_connect, n = 3)
    # get indices
    j <- i * 30
    k <- j + 21
    l <- k + 9

    station_name[(j+1): l] <- rep(name, 30)
    regine_main[(j+1): l] <- rep(regine, 30)

    ## PAST
    temp <- read.table(file_connect, nrows = 21)
    # Time appears as DD/MM-YYYY
    year <- substring(temp[,1], 7, 10)
    month <- substring(temp[,1], 4, 5)
    day <- substring(temp[,1], 1, 2)
    time_vec[(j+1):k] <- paste(year, "-", month, "-", day, sep = "")
    precip[(j+1):k] <- temp[, 2]
    temperature[(j+1):k] <- temp[, 3]
    snow_storage[(j+1):k] <- temp[, 4]
    modelled_raw[(j+1):k] <- temp[, 5]  # This is the simulated discharge during the past period without data assmilation
    modelled_corr[(j+1):k] <- rep(NA, 21)
    modelled_M50[(j+1):k] <- rep(NA, 21)
    modelled_P50[(j+1):k] <- rep(NA, 21)

    measured[(j+1):k] <- temp[, 6]

    # Skip the line that separate past and forecast
    readLines(file_connect, n = 1)
    ## FUTURE
    temp <- read.table(file_connect, nrows = 9)
    year <- substring(temp[,1], 7, 10)
    month <- substring(temp[,1], 4, 5)
    day <- substring(temp[,1], 1, 2)
    time_vec[(k+1):l] <- paste(year, "-", month, "-", day, sep = "")
    precip[(k+1):l] <- temp[, 2]
    temperature[(k+1):l] <- temp[, 3]
    snow_storage[(k+1):l] <- temp[, 4]
    modelled_raw[(k+1):l] <- temp[, 5]  # This is the simulated discharge during the forecast period without data assimilation
    modelled_corr[(k+1):l] <- temp[, 6] # This is the modelled forecast after correction by observed data
    modelled_M50[(k+1):l] <- temp[, 7]
    modelled_P50[(k+1):l] <- temp[, 8]
    measured[(k+1):l] <- rep(NA, 9)

    # skip 1 lines before next station
    readLines(file_connect, n = 1)

    station_line <- readLines(file_connect, n = 1)
    x <- grepl("Felt", station_line)

    # read station name
    name <- substring(station_line, 7)
    index <- which(station_ref_name == name)

    if (length(index) >= 1) {
      regine <- regine_ref_nb[index]
      # regine <- metadata$regine_main[index]
    } else {
      regine <- "NA"
    }

    # Break it we reach the end of the file
    if (length(x) == 0) {break}
    # current_line_old <- current_line
    i <- i + 1
  }

  HBV <- data.frame(regine.main = regine_main,
                    nbname = paste(regine_main, "-", station_name, sep = ""),
                    time = time_vec,
#                     Input_Precip = precip,
#                     Input_Temp = temperature,
#                     State_Snow = snow_storage,
#                     Runoff_SimRaw = modelled_raw,  # ASK WHY IT IS DIFFERENT FROM THE MAIN FILE. SHOULD WE PLOT IT?
#                     Runoff_SimCorr = modelled_corr,
                    Runoff_SimPrecipM50 = modelled_M50,
                    Runoff_SimPrecipP50 = modelled_P50)
                    # Runoff_Obs = measured)

  HBV[HBV == -10000.000] <- NA
  HBV[HBV == -9999.000] <- NA
  HBV <- tbl_df(HBV)
  invisible(HBV)

}

#' @title Read DDD modelling results
#' @param filename Full path and name of the file with DDD modelling results ("demodata/DDD24h2015R/24hres.txt" as default))
#' @return A dataframe with the modelling results and parameters
#' @import dplyr
#' @export

read_DDD <- function(filename = system.file("demodata/DDD24h2015R", "24hres.txt", package = "NVEDATA")) {

  # Get the regine numbers related to the station names in the HBV output file
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  station_ref_name <- station_ref$V5

  ## Reading DDD model results
  file_connect <- file(filename, open = "rt")

  regine_main <- c()
  station_name <- c()
  time_vec <- c()
  precip <- c()
  temperature <- c()
  snow_storage <- c()
  gw_storage <- c()
  soil_moisture <- c()
  modelled <- c()
  measured <- c()

  x <- TRUE
  i = 0

  station_line <-   substring(readLines(file_connect, n = 1),2)
  regine <- strsplit(station_line, " ")[[1]][1]
  name <- station_ref_name[which(regine_ref_nb == as.character(regine))]

  while (x == TRUE) {
    # get indices
    j <- i * 30
    k <- j + 30

    regine_main[(j+1):k] <- rep(regine, 30)
    station_name[(j+1):k] <- rep(as.character(name), 30)

    temp <- read.table(file_connect, nrows = 30)
    # Time appears as DD/MM-YYYY
    year <- temp[,1]
    month <- temp[,2]
    day <- temp[,3]
    time_vec[(j+1):k] <- paste(year, "-", month, "-", day, sep = "")
    precip[(j+1):k] <- temp[, 5]
    temperature[(j+1):k] <- temp[, 6]
    measured[(j+1):(j+20)] <- temp[1:20, 7]
    measured[(j+21):k] <- rep(NA, 10)
    modelled[(j+1):k] <- temp[, 8]
    snow_storage[(j+1):k] <- temp[, 10]
    gw_storage[(j+1):k] <- temp[, 11]
    soil_moisture[(j+1):k] <- temp[, 13]

    station_line <-   substring(readLines(file_connect, n = 1),2)
    # Break it we reach the end of the file
    if (length(station_line) == 0) {break}

    regine <- strsplit(station_line, " ")[[1]][1]
    name <- station_ref_name[which(regine_ref_nb == as.character(regine))]

    # current_line_old <- current_line
    i <- i + 1
  }

  DDD <- data.frame(regine.main = regine_main,
                    nbname = paste(regine_main, "-", station_name, sep = ""),
                    time = time_vec,
                    Input_Precip = precip,
                    Input_Temp = temperature,
                    State_Snow = snow_storage,
                    State_GW = gw_storage,
                    State_Soil = soil_moisture,
                    Runoff_DDD.Sim = modelled,
                    Runoff_DDD.Obs = measured)

  DDD <- tbl_df(DDD)

  # Transform -10000.00 values with NAs
  DDD[DDD == -10000] <- NA
  invisible(DDD)
}


#' @title Read Flomtabell.rap to get return levels for each station
#' @param filename Full path and name of the datafile (default is demodata/flomtabell.rap). This function gets data for all stations.
#' If needed, we could add a parameter to get only specific stations
#' @return NOTHING Saves the results to an .rda file
#' @export

read_flomtabell <- function(filename = system.file("demodata", "flomtabell.txt", package = "NVEDATA")) {

  # Get the regine numbers related to the station names in the HBV output file
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  station_ref_name <- station_ref$V5


  ## The following data are on the file:
  ##   station name, station number, Qm(obs), Q5(obs), Q50(obs), Qm(sim), Q5(sim), Q50(sim), area

  file_connect <- file(filename, open = "rt")
  dat <- read.table(file_connect, sep = ":", header = TRUE)


    # station_name <- dat[ , 2]
    regine_main <- dat[ , 3]
    station_name <- station_ref_name[regine_main]
    obs.1Y <- dat[ , 4]
    obs.5Y <- dat[ , 5]
    obs.50Y <- dat[ , 6]

    sim.1Y <- dat[ , 7]
    sim.5Y <- dat[ , 8]
    sim.50Y <- dat[ , 9]

    # area <- dat[ , 10]  # Let's not read the catchment area as those are already saved in the metadata file

  flomtabell <- data.frame(regine.main = regine_main,
                           nbname = paste(regine_main, "-", station_name, sep = ""),
                    Obs_1Y = obs.1Y,
                    Obs_5Y = obs.5Y,
                    Obs_50Y = obs.50Y,
                    Sim_1Y = sim.1Y,
                    Sim_5Y = sim.5Y,
                    Sim_50Y = sim.50Y
                    # Obs_Area = area
                    )

  # Transform -10000.00 values with NAs
  flomtabell[flomtabell == -10000.00] <- NA
  flomtabell[flomtabell == -9999.000] <- NA

  flomtabell <- tbl_df(flomtabell)
  invisible(flomtabell)

}



#' @title Read Flomtabell.rap to get return levels for each station
#' @param folder Path to the folder where all the individual files are. Default is demodata/HBV_past_year
#' @return NOTHING Saves the results to an .rda file
#' @import purrr
#' @import lubridate
#' @import dplyr
#' @export

read_past_HBV <- function(folder = system.file("demodata/HBV_past_year", package = "NVEDATA")) {

  # Get the regine numbers related to the station names in the HBV output file
  # Read it from the package for use anywhere
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  station_ref_name <- station_ref$V5

  ## The following data are on each file:
  ##   date          Qobs       Qsimobs        Qsim       Qusi
  file_sources = list.files(folder,
                            pattern="*.dat$", full.names=TRUE,
                            ignore.case=TRUE)

  read_past_HBV_single <- function(filename, regine_ref_nb, station_ref_name) {

    # read station name and find index to get back to regine_main number
    split_path <- strsplit(filename, "HBV_past_year/")
    split_filename <- strsplit(split_path[[1]][2], ".dat")
    name <- split_filename[[1]][1]
    index <- which(station_ref_name == as.character(name))
    print(as.character(split_path))
    print(as.character(split_filename))
    print(index)

    if (length(index) >= 1) {
      regine <- regine_ref_nb[index]
    } else {
      regine <- "NA"
    }

    file_connect <- file(filename, open = "rt")
    dat <- read.table(file_connect, sep = "", header = TRUE)

    time_vect <- lubridate::ymd(dat[ , 1])
    Qobs <- dat[ , 2]
    Qsimobs <- dat[ , 3]
    Qsim <- dat[ , 4]
    Qusi <- dat[ , 5]
    # station_name <- rep(station_name, length(time))
    regine <- rep(regine, length(time))
    station_name <- rep(name, length(time))

    HBV_past_year <- data.frame(regine.main = regine,
                                nbname = paste(regine, "-", station_name, sep = ""),
                                time = time_vect,
                                Runoff_Obs = Qobs,
                                Runoff_Sim.obs = Qsimobs,
                                Runoff_Sim.sim = Qsim,
                                Runoff_Sim.sim.corr = Qusi)
    # Transform -10000.00 values with NAs
    HBV_past_year[HBV_past_year == -9999.000] <- NA
    HBV_past_year[HBV_past_year == -10000.000] <- NA
    HBV_past_year <- dplyr::tbl_df(HBV_past_year)
  }

# purrr::map similar to apply but somehow easier to use in this case
  HBV_past_year <- purrr::map(file_sources, read_past_HBV_single, regine_ref_nb, station_ref_name)

  invisible(HBV_past_year)

}


# To implement with the http rest end-point


read_ODM <- function(path, filename) {

  ## Reading ODM model results. There is a folder per station.
  # Operational path is (for 1st station only):
  # /hdata/drift/flood/H-VEPS02/simu_hbv_elev_24h/AAMOT_ELEV_24h/InputTimeSeries.txt
  file_connect <- file("./data/flood/H-VEPS02/simu_hbv_elev_24h/AAMOT_ELEV_24h/InputTimeSeries.txt", open = "rt")
  readLines(file_connect, n = 6)
  # DATE / TEMP / PRECIP / TEMP / PRECIP / PRECIP / TEMP / DISCHARGE
  ODM <- read.table(file_connect, sep = "", fill = TRUE)
  colnames(ODM) <-  c("DATE", "TEMP", "PRECIP", "TEMP", "PRECIP", "PRECIP", "TEMP", "DISCHARGE")
}





