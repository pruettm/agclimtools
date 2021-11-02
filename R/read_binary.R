#' Reads a binary weather file
#'
#' @param file_path path to file location
#' @param num_vars number of variables in data set either 8 or 4 (default 8)
#' @param hist boolean representing time frame of data TRUE for historical
#' FALSE for future if missing hist will be detected in file path
#' @param id Either a string or 'NULL'. If a string, the output will contain
#' a variable with that name with the filename(s) as the value.
#' If 'NULL', the default, no variable will be created.
#' @return a data frame with date and either 4 or 8 environmental variables
#' @export
#'
read_binary <- function(file_path, num_vars = 8, hist, id = NULL){

  # set hist from file name if missing
  if (missing(hist)) {hist <- grepl("hist", file_path)}
  if (missing(num_vars)) {stop("Please set number of variables in file")}

   # Check hist and set date range
  if (hist){
    ymd_file <- data.frame(date = seq.Date(as.Date("1950/01/01"),
                                           as.Date("2005/12/31"),
                                           by = "day"))
  } else {
    ymd_file <- data.frame(date = seq.Date(as.Date("2006/01/01"),
                                           as.Date("2099/12/31"),
                                           by = "day"))
  }

  # Read Data
  Nrecords <- nrow(ymd_file)
  ind <- seq(1, Nrecords * num_vars, num_vars)
  temp <- readBin(file_path, integer(),
                  size = 2,
                  n = Nrecords * num_vars,
                  endian="little")
  dataM <- matrix(0, Nrecords, num_vars)

  # Scale values
  if (num_vars == 4) {
    dataM[1:Nrecords, 1] <- temp[ind] / 40.00       # precip data
    dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00  # Max temperature data
    dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00  # Min temperature data
    dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00  # Wind speed data

    data <- cbind(ymd_file, dataM)
    colnames(data) <- c("date", "precip", "tmax", "tmin", "windspeed")

  } else if (num_vars == 8) {
    dataM[1:Nrecords, 1] <- temp[ind] / 40.00         # precip data
    dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00    # Max temperature data
    dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00    # Min temperature data
    dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00    # Wind speed data
    dataM[1:Nrecords, 5] <- temp[ind + 4] / 10000.00  # SPH
    dataM[1:Nrecords, 6] <- temp[ind + 5] / 40.00     # SRAD
    dataM[1:Nrecords, 7] <- temp[ind + 6] / 100.00    # Rmax
    dataM[1:Nrecords, 8] <- temp[ind + 7] / 100.00    # RMin
    data <- cbind(ymd_file, dataM)
    colnames(data) <- c("date", "precip", "tmax", "tmin",
                           "windspeed", "SPH", "SRAD", "Rmax", "Rmin")
  }

  if (!is.null(id)) {
    data <- cbind(id = file_path, data)
    colnames(data)[1] <- id
  }

  return(data)
}


#' Reads a GRIDMET binary weather file
#'
#' @param file_path path to file location
#' @param begin beginning year of data
#' @param end ending year of data
#' @param id Either a string or 'NULL'. If a string, the output will contain
#' a variable with that name with the filename(s) as the value.
#' If 'NULL', the default, no variable will be created.
#' @return a data frame with date and 8 environmental variables
#' @export
#'
read_gridmet <- function(file_path, begin = 1979, end = 2019, id = NULL){


  ymd_file <- data.frame(date = seq.Date(as.Date(paste0(begin, "/01/01")),
                                         as.Date(paste0(end, "/12/31")),
                                         by = "day"))


  # Read Data
  Nrecords <- nrow(ymd_file)
  ind <- seq(1, Nrecords * 8, 8)
  temp <- readBin(file_path, integer(),
                  size = 2,
                  n = Nrecords * 8,
                  endian="little")
  dataM <- matrix(0, Nrecords, 8)
  dataM[1:Nrecords, 1] <- temp[ind] / 40.00         # precip data
  dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00    # Max temperature data
  dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00    # Min temperature data
  dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00    # Wind speed data
  dataM[1:Nrecords, 5] <- temp[ind + 4] / 10000.00  # SPH
  dataM[1:Nrecords, 6] <- temp[ind + 5] / 40.00     # SRAD
  dataM[1:Nrecords, 7] <- temp[ind + 6] / 100.00    # Rmax
  dataM[1:Nrecords, 8] <- temp[ind + 7] / 100.00    # RMin
  data <- cbind(ymd_file, dataM)
  colnames(data) <- c("date", "precip", "tmax", "tmin",
                      "windspeed", "SPH", "SRAD", "Rmax", "Rmin")

  if (!is.null(id)) {
    data <- cbind(id = file_path, data)
    colnames(data)[1] <- id
  }

  return(data)
}
