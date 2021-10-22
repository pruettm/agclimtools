#' Download MACA climate data from kamiak
#'
#' @param location file name of location to download in format "data_lat_long"
#' @param data_folder path to local folder to store downloaded data
#' @param kamiak_user_name username used to access kamiak ex. "user.name"
#' @param models list of models to download defaults to all models
#' @param climate_proj list of climate projections to download (historical, rcp45, rcp85)
#'
#' @export
#'
#' @examples \dontrun{download_maca("data_36.65625_-121.65625", "data", "matthew.pruett")}
download_maca <- function(location, data_folder, kamiak_user_name,
                          models = agclimtools::maca_models,
                          climate_proj = agclimtools::climate_projections){

  if (requireNamespace("ssh", quietly = TRUE)) {
    # check if data folder exist and create if missing
    if(!dir.exists(data_folder)) {dir.create(data_folder)}

    # connect to kamiak (prompts user password)
    session <- ssh::ssh_connect(paste0(kamiak_user_name, "@kamiak.wsu.edu"))

    # build folder structure
    tidyr::expand_grid(models, climate_proj) %>%
      dplyr::mutate(path = paste(data_folder, .data$models, .data$climate_proj, sep = "/")) %>%
      dplyr::pull(.data$path) %>%
      purrr::walk(function(x){if(!dir.exists(x)){dir.create(x, recursive = TRUE)}})


    df <- tidyr::expand_grid(models, climate_proj, location) %>%
      dplyr::mutate(local_path = file.path(data_folder, .data$models, .data$climate_proj, "."),
                    server_path = file.path(kamiak_maca_path(location), .data$models, .data$climate_proj, location))

    purrr::walk2(df$server_path, df$local_path, ssh::scp_download, session = session)

    ssh::ssh_disconnect(session)

  } else {
    print("Please install the ssh package to use this function.")
  }
}
