


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

load_runoff_all <-  function(data, path) {

  # Load file

  qdata <- read_runoff_file(path,data$metadata$obs_series)

  # Convert from m3/s to mm/day

  qdata$Value <- (qdata$Value * 86400 * 1000) / (data$metadata$area_total * 1e6)

  # Temporary data frame

  tmp <- dplyr::tbl_df(data.frame(Time = data$time_vec))
  tmp <- dplyr::left_join(tmp, qdata)

  # Add runoff to data structure

  data$Runoff <- tmp$Value

  return(data)

}


#' @title Read netdcf file
#' @param filename Name of the netcdf file
#' @return Vector with values (reshaped into a vector from a grid)
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


