#' CONUS CDL codes for each maca location
#'
#' A dataset containing 2020 Cropland Data Layer Codes for all CONUS locations
#'
#' @format A data frame with 4218330 rows and 2 variables:
#' \describe{
#'   \item{location}{file name of maca grid cell with format "data_lat_long"}
#'   \item{cdl_code_2020}{Crop Code from the 2020 cdl contained within each location}
#' }
#'
#' @source \url{https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php}
"maca_2020_cdl"

#'
#'
#' A list containing the names of 16 MACA models
#'
#' @format A list with 16 models
#'
#'
"maca_models"

#' Climate projections included with MACA
#'
#' A list with 3 climate projections historical, rcp45, rcp85
#'
#' @format A list with 3 climate projections
#'
"climate_projections"

