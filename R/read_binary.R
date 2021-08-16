#' Reads a binary weather file
#'
#' @param file_path path to file location
#' @param num_vars number of variables in data set either 8 or 4
#' @param hist boolean representing time frame of data TRUE for historical
#' FALSE for future if missing hist will be detected oin file path
#'
#' @return a matrix with date and either 4 or 8 environmental variables
#' @export
#'
read_binary <- function(file_path, num_vars, hist){

  if (missing(hist)) {hist <- grepl("hist", file_path)}

  if (hist){
    start_year <- 1950
    end_year <- 2005
  } else {
    start_year <- 2006
    end_year <- 2099
  }

  ymd_file <- data.frame(date = seq.Date(as.Date(paste0(start_year, "/01/01")),
                                         as.Date(paste0(end_year, "/12/31")),
                                         by = "day"))

  if (num_vars == 4) {
    data <- read_binary_addmdy_4var(file_path, ymd_file)
  } else if (num_vars == 8) {
    data <- read_binary_addmdy_8var(file_path, ymd_file)
  } else {
    stop("Please set number of variables in file")
  }
  return(data)
}

#' Read files with 4 variables
#'
#' @param filename path to file loaction
#' @param ymd date column
#'
#' @return binary data
#'
read_binary_addmdy_4var <- function(filename, ymd) {
  Nofvariables <- 4 # number of variables or column in the forcing data file
  Nrecords <- nrow(ymd)
  ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
  fileCon <- file(filename, "rb")
  temp <- readBin(fileCon, integer(),
                  size = 2,
                  n = Nrecords * Nofvariables,
                  endian="little")
  dataM <- matrix(0, Nrecords, 4)
  dataM[1:Nrecords, 1] <- temp[ind] / 40.00       # precip data
  dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00  # Max temperature data
  dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00  # Min temperature data
  dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00  # Wind speed data

  AllData <- cbind(ymd, dataM)
  colnames(AllData) <- c("date", "precip", "Tmax", "Tmin", "windspeed")
  close(fileCon)
  return(AllData)
}

#' Read files with 8 variables
#'
#' @param filename path to file loaction
#' @param ymd date column
#'
#' @return binary data
#'
read_binary_addmdy_8var <- function(filename, ymd){
  Nofvariables <- 8 # number of variables or column in the forcing data file
  Nrecords <- nrow(ymd)
  ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
  fileCon <- file(filename, "rb")
  temp <- readBin(fileCon, integer(),
                  size = 2,
                  n = Nrecords * Nofvariables,
                  endian = "little")
  dataM <- matrix(0, Nrecords, 8)
  dataM[1:Nrecords, 1] <- temp[ind] / 40.00         # precip data
  dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00    # Max temperature data
  dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00    # Min temperature data
  dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00    # Wind speed data
  dataM[1:Nrecords, 5] <- temp[ind + 4] / 10000.00  # SPH
  dataM[1:Nrecords, 6] <- temp[ind + 5] / 40.00     # SRAD
  dataM[1:Nrecords, 7] <- temp[ind + 6] / 100.00    # Rmax
  dataM[1:Nrecords, 8] <- temp[ind + 7] / 100.00    # RMin
  AllData <- cbind(ymd, dataM)
  colnames(AllData) <- c("date", "precip", "tmax", "tmin",
                         "windspeed", "SPH", "SRAD", "Rmax", "Rmin")
  close(fileCon)
  return(AllData)
}
