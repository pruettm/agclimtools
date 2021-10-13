#' Calculate temp after Sunrise
#'
#' @param hour hour of day
#' @param tmin minimum temperature of day
#' @param tmax maximum temperature of day
#' @param sunrise hour of sunrise
#' @param sunset hour of sunset
#'
#' @return temperature at hour
#' @export
#'
#' @examples temp_after_sunrise(hour = 12, tmin = 15, tmax = 33, sunrise = 8.15, sunset = 18.4)
temp_after_sunrise <-
  function(hour, tmin, tmax, sunrise, sunset){
    temp <-
      (tmax - tmin)*sin(pi*(hour-sunrise)/(sunset-sunrise+4)) + tmin

    return(temp)
  }

#' Calculate temp after sunset
#'
#' @param hour hour of day
#' @param tmin minimum temperature of day
#' @param tmax maximum temperature of day
#' @param tmin_next minimum temperature of next day
#' @param sunrise hour of sunrise
#' @param sunset hour of sunset
#' @param sunrise_next hour of sunrise for the next day
#'
#' @return temperature at hour
#' @export
#'
#' @examples temp_after_sunset(hour = 12, 15, 33, 17, 8.15, 18.4, 8.2)
temp_after_sunset <-
  function(hour, tmin, tmax, tmin_next, sunrise, sunset, sunrise_next){

    t_sunset <-
      temp_after_sunrise(sunset, tmin, tmax, sunrise, sunset)

    temp <-
      t_sunset -
      ((t_sunset - tmin_next)/log(24 - (sunset - sunrise_next) + 1))*log(hour - sunset + 1)

    return(temp)
  }

#' Calculate Hourly temperature for any hour of the day
#'
#' @param hour hour of day
#' @param tmin minimum temperature of day
#' @param tmax maximum temperature of day
#' @param tmin_prev minimum temperature of previous day
#' @param tmax_prev maximum temperature of previous day
#' @param tmin_next minimum temperature of next day
#' @param sunrise hour of sunrise
#' @param sunset hour of sunset
#' @param sunrise_prev hour of sunrise for the previous day
#' @param sunset_prev hour of sunset for the previous day
#' @param sunrise_next hour of sunrise for the next day
#'
#' @return temperature at hour
#' @export
#'
#' @examples hourly_temp(0:23, 5, 20, 5, 20, 5, 8, 18, 8, 18, 8)
hourly_temp <-
  function(hour, tmin, tmax, tmin_prev, tmax_prev,
           tmin_next, sunrise, sunset, sunrise_prev,
           sunset_prev, sunrise_next){

    t_before_sunrise <- temp_after_sunset(hour + 24, tmin_prev, tmax_prev,
                      tmin, sunrise_prev, sunset_prev, sunrise)

    t_after_sunrise <- temp_after_sunrise(hour, tmin, tmax, sunrise, sunset)

    t_after_sunset <- temp_after_sunset(hour, tmin, tmax, tmin_next,
                      sunrise, sunset, sunrise_next)

    temp <- rep(NA, length(hour))

    temp[hour <= sunrise] <- t_before_sunrise[hour <= sunrise]
    temp[hour <= sunset] <- t_after_sunrise[hour <= sunset]
    temp[hour > sunset] <- t_after_sunset[hour > sunset]

    return(temp)
  }

#' calculate sunrise and sunset
#'
#' @param doy day of year
#' @param lat latitude (degrees)
#'
#' @return data frame with columns sunrise and sunset
#' @export
#'
#' @examples sunrise_sunset(doy = 1:366, lat = 45)
#'
sunrise_sunset <- function(doy, lat){

  gamma <-  2*pi/365*((doy)-1)

  delta <- 180/pi*(0.006918 - 0.399912*cos(gamma) +0.070257*sin(gamma) -
              0.006758*cos(gamma) + 0.000907*sin(gamma) -
              0.002697*cos(3*gamma) + 0.00148*sin(3*gamma))

  cos_wo <- (sin(-0.8333/360*2*pi) - sin(lat/360*2*pi)*sin(delta/360*2*pi))/
    (cos(lat/360*2*pi)*cos(delta/360*2*pi))

  sunrise <- 12 - acos(cos_wo)/(15/360*2*pi)
  sunset <- 12 + acos(cos_wo)/(15/360*2*pi)

  return(data.frame(sunrise, sunset))
}


#' Add sunrise and sunset times to data frame
#'
#' @param data data frame with day of year column named 'doy'
#' @param lat location latitude (degrees)
#'
#' @return data frame with added columns sunrise and sunset
#' @export
#'
#' @examples add_sunrise_sunset(data.frame(doy = 1:366), lat = 45)
#'
#' @importFrom rlang .data
add_sunrise_sunset <- function(data, lat){
  data %>%
    dplyr::mutate(sunrise_sunset(.data$doy, lat))
}

#' Add hourly temps to data frame
#'
#' @param data data frame with columns date, tmin, and tmax
#' @param lat location latitude
#' @param hours either single value or list of hours of the day to calculate
#'
#' @return data frame with added columns hour and temp
#' @export
#'
#' @examples data.frame(date = seq(as.Date("2000/1/1"), as.Date("2000/1/3"), "days"),
#' tmin = c(1, 2, 6), tmax = c(10, 12, 15)) %>% add_hourly_temps(lat = 45, hours = 0:23)
#'
add_hourly_temps <- function(data, lat, hours) {

  n_hours <- length(hours)
  n_days <- nrow(data)

  all_hours <- rep.int(hours, n_days)

  doy <-  lubridate::yday(data$date)
  sunrise_sunset <- sunrise_sunset(doy, lat)
  sunrise <- rep(sunrise_sunset$sunrise, each = n_hours)
  sunset <- rep(sunrise_sunset$sunset, each = n_hours)
  tmin <- rep(data$tmin, each = n_hours)
  tmax <- rep(data$tmax, each = n_hours)

  temp <-
    suppressWarnings(
    hourly_temp(hour = all_hours,
                tmin = tmin,
                tmax = tmax,
                tmin_prev = dplyr::lag(tmin, n = n_hours),
                tmax_prev = dplyr::lag(tmax, n = n_hours),
                tmin_next = dplyr::lead(tmin, n = n_hours),
                sunrise = sunrise,
                sunset = sunset,
                sunrise_prev = dplyr::lag(sunrise, n = n_hours),
                sunset_prev = dplyr::lag(sunset, n = n_hours),
                sunrise_next = dplyr::lead(sunrise, n = n_hours))
    )

  data %>%
    tidyr::expand_grid(hour = hours) %>%
    dplyr::mutate(temp = temp)

}
