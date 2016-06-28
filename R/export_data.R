

#' @title Write data to text file
#' @param data List with data obtained using functions load_data_mean or load_data_elev
#' @param path Folder for storing the data
#' @examples
#' library(lubridate)
#' path_met <- '//hdata/grid/metdata/met_obs_v2.0'
#' path_runoff <- '//hdata/fou/Vannbalansekart/Data/Runoff_All'
#' regine_main <- c('1.48','1.49','1.50')
#' time_vec <- seq(ymd("2011-01-01"), ymd("2011-01-04"), by = "days")
#' res <- load_data_mean(path_met, path_runoff, regine_main, time_vec)
#' path <- 'C:/Users/psan/Desktop/NVES_BESTA_DATA'
#' write_to_text(res, path)
#' @export

write_to_text <- function(data, path) {

  write_one_station <- function(data, path) {

    df <- data.frame(Time = data$time_vec,
                     Tair = round(data$Tair, digits = 2),
                     Prec = round(data$Prec, digits = 2),
                     Runoff = round(data$Runoff, digits = 2))

    filename <- paste(data$regine_main,"_data.txt", sep = "")

    write.table(df, file = paste(path, "/", filename, sep = ""), quote = FALSE, row.names = FALSE)

  }

  lapply(data, write_one_station, path = path)

}


