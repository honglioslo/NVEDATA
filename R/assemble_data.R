

#' @title Load meteorological data for a set of stations
#' @param path Path to folder with BIL files
#' @param station Vector with stations
#' @param time_vec Vector with dates
#' @examples
#' path = "//hdata/grid/metdata/met_obs_v2.0"
#' station = c("1.48","1.49","1.50")
#' time_vec = seq(as.Date("2011-01-01"), as.Date("2011-01-04"), by="days")
#' res = load_data_mean(path,station,time_vec)
#'
#' @return A list with data
#' @export

load_data_mean = function(path,station,time_vec) {

  # Initilize list for one station

  init_list = function(istat) {

    data_all[[istat]] = list(time_vec = time_vec,
                             station = station[istat],
                             Tair = NA,
                             Prec = NA)

  }

  # Assign variable to general data structure

  assign_var = function(istat,iday,variable) {

    data_all[[istat]][[variable]][iday] = met_data[[istat]]
    data_all[[istat]]

  }

  # Initlize data structure

  nstations = length(station)

  data_all = vector("list",nstations)

  data_all = lapply(seq_along(data_all),init_list)

  # Loop over time

  for (iday in seq_along(time_vec)) {

    print(paste("Processing day ",iday,sep=""))


    # Process air temperature data

    filename = paste(path,"/tm/",format(time_vec[iday],"%Y"),"/tm_",format(time_vec[iday],"%Y_%m_%d"),".bil",sep="")

    met_data = read_bil_file(filename)

    # Unit conversions

    met_data = met_data/10 - 273.15

    # Extract data for stations

    met_data = lapply(station,extract_grid_data,grid_data=met_data)

    # Average data

    met_data = lapply(met_data,mean)

    # Assign variable to data structure

    data_all = lapply(seq_along(data_all),assign_var,iday=iday,variable="Tair")


    # Process precipitation data

    filename = paste(path,"/rr/",format(time_vec[iday],"%Y"),"/rr_",format(time_vec[iday],"%Y_%m_%d"),".bil",sep="")

    met_data = read_bil_file(filename)

    # Unit conversions

    met_data = met_data/10

    # Extract data for stations

    met_data = lapply(station,extract_grid_data,grid_data=met_data)

    # Average data

    met_data = lapply(met_data,mean)

    # Assign variable to data structure

    data_all = lapply(seq_along(data_all),assign_var,iday=iday,variable="Prec")

  }

  return(data_all)

}


