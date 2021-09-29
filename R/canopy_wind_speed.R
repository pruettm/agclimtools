#' Calculate Windspeed within canopy
#'
#' @param wind_speed Wind speed at 10 m height (m/s)
#' @param canopy_height Height of canopy (m), default 3 m
#' @param fruit_height Height of fruit  (m), default 2 m
#' @param a Constant that determines rates of attenuation within canopy, default is 0.4 for fruit trees
#'
#' @return Wind speed at fruit height in canopy (m/s)
#' @export
#'
#' @examples canopy_wind_speed(1)
#'
canopy_wind_speed <- function(wind_speed, canopy_height=3, fruit_height=2, a = 0.4){
  u_star <- (0.4*wind_speed)/log((10 - .65*canopy_height)/(.1*canopy_height))
  windspeed_canopy <- (u_star/0.4)*log(3.5)
  windspeed_fruit <- windspeed_canopy*exp(a*((fruit_height/canopy_height)-1))
  return(windspeed_fruit)
}
