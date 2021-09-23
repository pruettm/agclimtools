#' Get Kamiak File path to MACA data
#'
#' @param location file name of location to download in format "data_lat_long"
#'
#' @return base path to file in kamiak
#' @export
#'
#' @examples kamiak_maca_path("data_36.65625_-121.65625")
kamiak_maca_path <- function(location){

  name_split <- stringr::str_split(location, pattern = "_", simplify = TRUE)

  lat <- as.numeric(name_split[,2])
  lon <- as.numeric(name_split[,3])

  kamiak_path <- dplyr::if_else(lon <= -109.0312 & lat >= 32.03125,
                                "/data/adam/data/metdata/VIC_ext_maca_v2_binary_westUSA",
                                "/data/project/agaid/US_Conus")
  return(kamiak_path)

}
