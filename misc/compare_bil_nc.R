# Compare methods for reading bil and nc files

# Read netcdf file

require(ncdf4)

file <- "//hdata/grid/metdata/met_obs_v2.0/rr/1966/rr_1966_01_02.nc"

fnc = nc_open(file)

v1<-fnc$var[[1]]

prec_nc <- ncvar_get(fnc,v1)
prec_nc <- as.vector(prec_nc)

nc_close(fnc)

# Read bil file

read_bil_file <- function(filename, nrow = 1550, ncol = 1195) {

  fid <- file(filename, "rb")

  grid_data <- readBin(fid, n = nrow * ncol, what = integer(), size = 2, signed = FALSE, endian = "little")

  close(fid)

  grid_data[grid_data == 65535] <- NA

  return(grid_data)

}

file <- "//hdata/grid/metdata/met_obs_v2.0/rr/1966/rr_1966_01_02.bil"

prec_bil <- read_bil_file(file)

prec_bil <- as.numeric(prec_bil/10)

# Compare results

max(abs(prec_nc-prec_bil),na.rm=T)

cor(x = prec_nc, y = prec_bil, use = "complete.obs")


