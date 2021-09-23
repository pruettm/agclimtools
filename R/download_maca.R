#' Download MACA climate data from kamiak
#'
#' @param location file name of location to download in format "data_lat_long"
#' @param data_folder path to local folder to store downloaded data
#' @param kamiak_user_name username used to access kamiak ex. "user.name"
#'
#' @export
#'
#' @examples \dontrun{download_maca("data_36.65625_-121.65625", "data", "matthew.pruett")}
download_maca <- function(location, data_folder, kamiak_user_name){

  # check if data folder exist and create if missing
  if(!dir.exists(data_folder)) {dir.create(data_folder)}

  # connect to kamiak (prompts user password)
  session <- ssh::ssh_connect(paste0(kamiak_user_name, "@kamiak.wsu.edu"))

  # define all models and climate projections
  models <- c("bcc-csm1-1", "BNU-ESM", "CanESM2", "CNRM-CM5", "CSIRO-Mk3-6-0",
              "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC365", "HadGEM2-ES365",
              "inmcm4",  "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR",
              "MIROC5",  "MIROC-ESM", "MIROC-ESM-CHEM")

  climate_proj <- c("historical", "rcp45", "rcp85")

  # build folder structure
  tidyr::expand_grid(models, climate_proj) %>%
    dplyr::mutate(path = paste(data_folder, models, climate_proj, sep = "/")) %>%
    dplyr::pull(.data$path) %>%
    purrr::walk(function(x){if(!dir.exists(x)){dir.create(x, recursive = TRUE)}})


  df <- tidyr::expand_grid(models, climate_proj, location) %>%
    dplyr::mutate(local_path = file.path(data_folder, models, climate_proj, "."),
                  server_path = file.path(kamiak_maca_path(location), models, climate_proj, location))

  purrr::walk2(df$server_path, df$local_path, ssh::scp_download, session = session)

  ssh::ssh_disconnect(session)

}
