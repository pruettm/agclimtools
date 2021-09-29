#' Dew Point Temperature based on air temperature and relative humidity
#'
#' @param relative_humidity Relative humidity, ranges from zero to one
#' @param temp Air temperature (°C)
#'
#' @return Dew point temperature (°C)
#' @export
#'
#' @examples dew_point(0.75, 35)
dew_point <- function(relative_humidity, temp){
  # https://en.wikipedia.org/wiki/Dew_point
  a = 6.1121 #mbar
  b = 18.678
  c = 257.14 #°C
  d = 234.5 #°C.

  gamma <- log(relative_humidity/100) +
    (b*temp)/(c+temp)

  T_dew <- (c*gamma)/(b-gamma)
  return(T_dew)
}
