#' Clear Sky Radiation from Allen et al.
#'
#' @param hour hour of day
#' @param doy day of year
#' @param lat latitude
#' @param long longitude
#' @param time_zone_long longitude of time zone
#' @param elevation elevation (meters)
#' @param freq_h frequency of observations in hours defaults to 1 hour (0.5 = half hour)
#'
#' @return Clear sky radiation in W/m2
#' @export
#'
#' @examples clear_sky(0:23, 34, 45, -119, -120, 10)
clear_sky <- function(hour, doy, lat, long, time_zone_long, elevation, freq_h = 1){
  t_mid <- hour - freq_h/2
  b <- 2*pi*(doy - 81)/364
  Sc <- 0.1645*sin(2*b) - 0.1255*cos(b) - 0.025*sin(b)
  w <- pi/12*((t_mid+0.06667*(time_zone_long - long) + Sc) - 12)
  w1 <- w - pi*freq_h/24
  w2 <- w + pi*freq_h/24

  long_r <- long*pi/180
  lat_r <- lat*pi/180
  dr <- 1 + 0.033*cos(2*pi/365*doy)
  SD <- 0.4093*sin(2*pi/365*doy - 1.39)

  Ra <- 12*60/pi*0.082*dr*((w2 - w1)*sin(lat_r)*sin(SD) +
                             cos(lat_r)*cos(SD)*(sin(w2) - sin(w1)))
  Ra <- 1000000/60/60/freq_h*Ra

  P <- 101.3*(((293 - 0.0065*elevation)/293)^5.26)
  sinPhi <- sin(lat_r)*sin(SD) + cos(lat_r)*cos(SD)*cos(w)
  Kt <- 0.95
  Rso <- Ra*exp(-0.0021*P/(Kt*sinPhi))

  Rso <- ifelse(Rso < 0, 0, Rso)
  return(Rso)
}

#' Extraterrestrial Solar Radiation from Allen et al.
#'
#' @param hour hour of day
#' @param doy day of year
#' @param lat latitude
#' @param long longitude
#' @param time_zone_long longitude of time zone
#' @param freq_h frequency of observations in hours defaults to 1 hour (0.5 = half hour)
#'
#' @return Extraterrestrial solar radiation in W/m^2
#' @export
#'
#' @examples extrat(0:23, 34, 45, -119, -120)
extrat <- function(hour, doy, lat, long, time_zone_long, freq_h = 1){
  t_mid <- hour - freq_h/2
  b <- 2*pi*(doy - 81)/364
  Sc <- 0.1645*sin(2*b) - 0.1255*cos(b) - 0.025*sin(b)
  w <- pi/12*((t_mid+0.06667*(time_zone_long - long) + Sc) - 12)
  w1 <- w - pi*freq_h/24
  w2 <- w + pi*freq_h/24

  long_r <- long*pi/180
  lat_r <- lat*pi/180
  dr <- 1 + 0.033*cos(2*pi/365*doy)
  SD <- 0.4093*sin(2*pi/365*doy - 1.39)

  Ra <- 12*60/pi*0.082*dr*((w2 - w1)*sin(lat_r)*sin(SD) +
                             cos(lat_r)*cos(SD)*(sin(w2) - sin(w1)))
  Ra <- 1000000/60/60/freq_h*Ra

  Ra <- ifelse(Ra < 0, 0, Ra)

  return(Ra)
}
