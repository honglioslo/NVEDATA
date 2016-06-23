

#' @title Read NC file
#' @param filename Name of the netcdf file
#' @return Vector with values (reshaped into a vector from a grid)
#' @export

read_nc_file <- function(filename) {

  fnc = ncdf4::nc_open(filename)

  v1<-fnc$var[[1]]

  data_nc <- ncdf4::ncvar_get(fnc,v1)
  data_nc <- as.vector(data_nc)

  ncdf4::nc_close(fnc)

  return(data_nc)

}


#' @title Read BIL file
#' @param filename Name of the BIL file
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


#' @title Extract data for a single watershed
#' @param regine_main Name of a station given as 'regine_area.main_no' (e.g. '1.48')
#' @param grid_data Data from one BIL file stored as a vector
#' @return Vector of values for one watershed
#' @export

load_single_wsh <- function(regine_main, grid_data) {

  irow <- which(meta_data$regine_main == regine_main)

  drainage_basin_key <- as.character(meta_data$drainage_basin_key[irow])

  return(grid_data[unlist(wsh_index[drainage_basin_key])])

}




