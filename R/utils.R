
#' @title Read BIL file
#' @param filename Name of BIL file
#' @param nrow Number of rows in grid file
#' @param ncol Number of columns in grid file
#' @return Vector with values from the BIL file
#' @export

read_bil_file = function(filename,nrow = 1550,ncol = 1195) {

  fid = file(filename,"rb")

  grid_data = readBin(fid, n = nrow*ncol, what= integer(), size = 2, signed=FALSE, endian = "little")

  close(fid)

  grid_data[grid_data==65535] = NA

  return(grid_data)

}


#' @title Extract data for a single watershed
#' @param station Name of a station (e.g. "1.48")
#' @param grid_data Data from one BIL file
#' @return Vector of values for one watershed
#' @export

extract_grid_data = function(station,grid_data) {

  irow = which(meta_data$station == station)

  drainage_basin_key = as.character(meta_data$drainage_basin_key[irow])

  return(grid_data[unlist(wsh_index[drainage_basin_key])])

}




