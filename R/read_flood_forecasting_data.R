## The following functions are authored by Florian Kobierska Baffie.
## They are used specifically by the Flood forecasting app (see https://github.com/fbaffie/Flood_forecasting)

#' @title Load flood data for a set of stations
#' @description Loads flood forecasting data for all available models
#' This function concatenates the results of all other functions defined in the file "read_flood_forecasting_data.R".
#' It is the only function from the NVEDATA package to be called directly by the flood forecasting app
#'
#' @param regine_main Vector of station numbers (regine_area and main_no separated by a full stop: see example).
#' Default is meta_data$regine_main from the packaged meta_data
#' @param HBV_2014_filename File with HBV_2014 results
#' Default is the network location: '//hdata/drift/flom/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt'
#' @param HBV_2016_filename File with HBV_2016 results
#' Default is the network location: '//hdata/drift/flom/usikkerhet_grd/ut_test/vfpost_usikkerhet.txt'
#' @param HBV_P_filename File with HBV_2016 precipitation correction results
#' Default is the network location: '//hdata/drift/flom/usikkerhet_grd/ut_test/vfp3030.txt'
#' @param DDD_filename File with DDD results
#' Default is the network location: '//hdata/drift/flom//DDD24h2015R/24hres.txt'
#' @examples
#' # For a specific set of stations:
#' regine_main <- c('1.48','1.49','1.50')
#' load_flood_data(regine_main)
#' # Or to run for every station:
#' load_flood_data()
#' # The following files should now be available in your working directory:
#' data_all.RData, meta_data.rda, HBV_2014.RData, HBV_2016.RData, HBV_past_year.RData, DDD.RData and flomtabell.RData
#' @return NULL The function saves several .rda files to the working directory
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' @importFrom tidyr %>%
#' @importFrom dplyr right_join
#' @importFrom dplyr filter
#' @importFrom reshape merge_all
#' @export

load_flood_data <- function(regine_main = meta_data$regine_main,
                            HBV_2014_filename = '//hdata/drift/flom/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt',
                            HBV_2016_filename = '//hdata/drift/flom/usikkerhet_grd/ut_test/vfpost_usikkerhet.txt',
                            HBV_P_filename = '//hdata/drift/flom/usikkerhet_grd/ut_test/vfp3030.txt',
                            DDD_filename = '//hdata/drift/flom//DDD24h2015R/24hres.txt') {

  # Test if metadata exist for stations
  if (!all(regine_main %in% meta_data$regine_main)) {

    imissing <- !(regine_main %in% meta_data$regine_main)

    station_str <- paste(regine_main[imissing], collapse = ", ")

    stop(paste("Lacking metadata for station", station_str, sep = " "))

  }

  ## HBV DATA READ FROM THE NVE NETWORK

  HBV_2014 <- read_HBV_data(HBV_2014_filename)

  HBV_2016_INIT <- read_HBV_data(HBV_2016_filename)
  # Elin doesn't want to plot L50, H50, L90, H90
  HBV_2016_INIT <- subset(HBV_2016_INIT, select = - c(Runoff_SimL50, Runoff_SimH50, Runoff_SimL90, Runoff_SimH90))
  HBV_2016_PRECIP_CORRECTION <- read_HBV_P(HBV_P_filename)

  HBV_past_year <- read_past_HBV()
  # Reshape the HBV_past_year into a dataframe. This is brutal and slow but allows having the same structure as the previous data
  HBV_past_year <- reshape::merge_all(HBV_past_year)
  HBV_past_year <- tidyr::gather(HBV_past_year, key = Tmp, value = Values, -time, -regine.main, -nbname) %>%
    tidyr::separate(Tmp, into = c("Type", "Variable"), sep = "_")

  # Create the long data frame to be used by ggplot in the flood forecasting app
  HBV_2014 <- tidyr::gather(HBV_2014, key = Tmp, value = Values, -time, -regine.main, -station.name, -nbname) %>%
    tidyr::separate(Tmp, into = c("Type", "Variable"), sep = "_")

  HBV_2016 <- dplyr::right_join(HBV_2016_INIT, HBV_2016_PRECIP_CORRECTION, by = c("regine.main", "time", "nbname"))
  HBV_2016 <- tidyr::gather(HBV_2016, key = Tmp, value = Values, -time, -regine.main, -station.name, -nbname) %>%
    tidyr::separate(Tmp, into = c("Type", "Variable"), sep = "_")

  ## DDD DATA READ FROM THE NVE NETWORK

  DDD <- read_DDD(DDD_filename)
  DDD <- tidyr::gather(DDD, key = Tmp, value = Values, -time, -regine.main, -nbname) %>%
    tidyr::separate(Tmp, into = c("Type", "Variable"), sep = "_")

  ## Read flood return levels

  flomtabell <- read_flomtabell()
  flomtabell <- tidyr::gather(flomtabell, key = Tmp, value = Values, -regine.main, -nbname) %>%
    tidyr::separate(Tmp, into = c("Type", "Variable"), sep = "_")
  flomtabell$Values <- as.numeric(flomtabell$Values)  # Maybe not required...



  # Initilize list for one station
  init_list <- function(regine_main_in) {

    data_all <- list(regine_main = regine_main_in,
                     metadata = dplyr::filter(meta_data, regine_main == regine_main_in),
                     # DDM = dplyr::filter(DDM_data, regine_main == regine_main_in),  # DDM TO BE DONE WITH BYMAN
                     DDD = dplyr::filter(DDD, regine.main == regine_main_in),
                     # ODM = dplyr::filter(ODM_data, regine_main == regine_main_in),  # ODM TO HAVE A SPECIFIC API FUNCTION
                     HBV_2014 = dplyr::filter(HBV_2014, regine.main == regine_main_in),
                     HBV_2016 = dplyr::filter(HBV_2016, regine.main == regine_main_in))

  }

  # Fill up with all stations specified in input argument (lapply over level 1, all stations)
  data_all <- lapply(regine_main, init_list)

  # Save the files to the working directory for later use in Shiny app
  save(data_all, file = paste(getwd(),"/","data_all.RData", sep = ""))
  save(meta_data, file = paste(getwd(),"/","meta_data.rda", sep = ""))
  save(HBV_2014, file = paste(getwd(),"/","HBV_2014.RData", sep = ""))
  save(HBV_2016, file = paste(getwd(),"/","HBV_2016.RData", sep = ""))
  save(HBV_past_year, file = paste(getwd(),"/", "HBV_past_year.RData", sep = ""))
  save(DDD, file = paste(getwd(),"/", "DDD.RData", sep = ""))
  save(flomtabell, file = paste(getwd(),"/", "flomtabell.RData", sep = ""))

}



#' @title Read HBV modelling results
#' @param filename Full path and name of the file with HBV modelling results
#' Default is "demodata/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt"
#' @examples
#' HBV <- read_HBV_data()
#' @return HBV A dataframe with the modelling results, input and state parameters
#' @importFrom dplyr tbl_df
#' @export

read_HBV_data <- function(filename = system.file("demodata/usikkerhet_grd/utskrift", "vfpost_usikkerhet.txt", package = "NVEDATA")) {

  # Get the regine numbers related to the station names in the HBV output file
  # Read it from the package for use anywhere
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  station_ref_name <- station_ref$V5

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
#' @param filename Full path and name of the file with HBV_2016 results with +-50% precipitation correction
#' Default is "demodata/usikkerhet_grd/ut_test/vfp3030.txt"
#' @examples
#' HBV_P <- read_HBV_P()
#' @return HBV A dataframe with modelling results
#' @export

read_HBV_P <- function(filename = system.file("demodata/usikkerhet_grd/ut_test", "vfp3030.txt", package = "NVEDATA")) {

  # Get the regine numbers related to the station names in the HBV output file
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  station_ref_name <- station_ref$V5

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
#' @param filename Full path and name of the file with DDD modelling results
#' Defaults is "demodata/DDD24h2015R/24hres.txt"
#' @examples
#' DDD <- read_DDD()
#' @return DDD A dataframe with modelling results, input and model state parameters
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
                    Runoff_Obs = measured)

  DDD <- tbl_df(DDD)

  DDD[DDD == -10000] <- NA
  invisible(DDD)
}


#' @title Read Flomtabell.rap to get return levels for each station
#' @param filename Full path and name of the datafile
#' Default is "demodata/flomtabell.rap" which has data for all stations in Norway
#' If needed, we could add a parameter to get only specific stations
#' @examples
#' flomtabell <- read_flomtabell()
#' @return flomtabell A dataframe with flood return levels
#' @export

read_flomtabell <- function(filename = system.file("demodata", "flomtabell.txt", package = "NVEDATA")) {

  ## The following data are on the file:
  ##   station name, station number, Qm(obs), Q5(obs), Q50(obs), Qm(sim), Q5(sim), Q50(sim), area

  file_connect <- file(filename, open = "rt")
  dat <- read.table(file_connect, sep = ":", header = FALSE, dec =",")
  # dec = "," is a trick to have 2.11 as a character


  station_name <- dat[ , 2]
  regine_main <- as.character(dat[ , 3])
  obs.mean <- dat[ , 4]
  obs.5Y <- dat[ , 5]
  obs.50Y <- dat[ , 6]

  sim.mean <- dat[ , 7]
  sim.5Y <- dat[ , 8]
  sim.50Y <- dat[ , 9]

  # area <- dat[ , 10]  # Let's not read the catchment area as those are already saved in the metadata file

  flomtabell <- data.frame(regine.main = regine_main,
                           nbname = paste(regine_main, "-", station_name, sep = ""),
                           Obs_mean = obs.mean,
                           Obs_5Y = obs.5Y,
                           Obs_50Y = obs.50Y,
                           Sim_mean = sim.mean,
                           Sim_5Y = sim.5Y,
                           Sim_50Y = sim.50Y
                           # Obs_Area = area
  )

  flomtabell[flomtabell == "-10000.00"] <- NA

  flomtabell <- tbl_df(flomtabell)
  invisible(flomtabell)

}



#' @title Read HBV results of the past year for each station
#' @param folder Path to the folder where all the individual files are.
#' Default is "demodata/HBV_past_year"
#' @examples
#' HBV_past_year <- read_past_HBV()
#' @return HBV_past_year A dataframe with HBV modelling results for the past year
#' Runoff_Obs is the observed discharge
#' Runoff_Sim.obs is the modelled discharge using observed meteorological data
#' Runoff_Sim.sim is the modelled discharge using raw forecast meteorological data
#' Runoff_Sim.sim.corr is the modelled discharge using corrected forecast meteorological data
#' @importFrom purrr map
#' @importFrom lubridate ymd
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
    regine <- rep(regine, length(time))
    station_name <- rep(name, length(time))

    HBV_past_year <- data.frame(regine.main = regine,
                                nbname = paste(regine, "-", station_name, sep = ""),
                                time = time_vect,
                                Runoff_Obs = Qobs,
                                Runoff_Sim.obs = Qsimobs,
                                Runoff_Sim.sim = Qsim,
                                Runoff_Sim.sim.corr = Qusi)

    HBV_past_year[HBV_past_year == -9999.000] <- NA
    HBV_past_year[HBV_past_year == -10000.000] <- NA
    HBV_past_year <- dplyr::tbl_df(HBV_past_year)
  }

  # purrr::map similar to apply but somehow easier to use in this case
  HBV_past_year <- purrr::map(file_sources, read_past_HBV_single, regine_ref_nb, station_ref_name)

  invisible(HBV_past_year)

}




## Below are functions that are not yet implemented:
## TODO: implement rest end-point reading function to get ODM results


# Wind speed data from Mannen
# http://h-web01.nve.no/chartserver/ # defines server
#   ShowData.aspx? # defines function(?)
# req=getchart& # defines request
#   ver=1.0& # defines version number
#   vfmt=json& # defines output format
#   time=20130422T0000;20130521T0000& # defines time interval
#   chs=10x10& # defines chart size
#   lang=no& # defines language
#   chlf=desc& # defines chart legend format
#   chsl=0;+0& # marks an area dependent on start and end dates/times
#   chd=ds=htsre,da=29,id=61410.16,rt=1:00,cht=line,mth=inst| # ???
#   ds=htsry,id=metx[61410;16].6038,mth=inst,rt=1:00,cht=line&nocache=0.33931976436234856 # ???

# read_ODM <- function(path, filename) {
#
#   ## Reading ODM model results. There is a folder per station.
#   # Operational path is (for 1st station only):
#   # /hdata/drift/flood/H-VEPS02/simu_hbv_elev_24h/AAMOT_ELEV_24h/InputTimeSeries.txt
#   file_connect <- file("./data/flood/H-VEPS02/simu_hbv_elev_24h/AAMOT_ELEV_24h/InputTimeSeries.txt", open = "rt")
#   readLines(file_connect, n = 6)
#   # DATE / TEMP / PRECIP / TEMP / PRECIP / PRECIP / TEMP / DISCHARGE
#   ODM <- read.table(file_connect, sep = "", fill = TRUE)
#   colnames(ODM) <-  c("DATE", "TEMP", "PRECIP", "TEMP", "PRECIP", "PRECIP", "TEMP", "DISCHARGE")
# }





