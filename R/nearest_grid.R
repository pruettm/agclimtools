#' return file name of nearest grid point based on 1/16 degree grid
#'
#' @param lat input latitude (degrees)
#' @param lon input longitude (degrees)
#'
#' @return file name of nearest grid point
#' @export
#'
#' @examples nearest_grid(46.3758, -123.4567)
nearest_grid <- function(lat, lon){

  lat_min <- 25.03125
  lon_min <- -80.40625

  lat_steps <- round((lat - lat_min) / (1/16))
  lon_steps <- round((lon - lon_min) / (1/16))

  lat_out <- sprintf("%06.5f", lat_steps*(1/16) + lat_min)
  lon_out <- sprintf("%06.5f", lon_steps*(1/16) + lon_min)

  file_path <- paste("data", lat_out, lon_out, sep = "_")

  return(file_path)
}
