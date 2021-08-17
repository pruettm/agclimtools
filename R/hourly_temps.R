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

    temp <-
      dplyr::case_when(hour <= sunrise ~
                  temp_after_sunset(hour + 24, tmin_prev, tmax_prev, tmin,
                                    sunrise_prev, sunset_prev, sunrise),
                hour <= sunset ~
                  temp_after_sunrise(hour, tmin, tmax, sunrise, sunset),
                hour > sunset ~
                  temp_after_sunset(hour, tmin, tmax, tmin_next,
                                    sunrise, sunset, sunrise_next)) |>
      suppressWarnings()

    return(temp)
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
  data |>
    dplyr::mutate(gamma = 2*pi/365*((.data$doy)-1),
           delta =
             180/pi*(0.006918 - 0.399912*cos(.data$gamma) +0.070257*sin(.data$gamma) -
                       0.006758*cos(.data$gamma) + 0.000907*sin(.data$gamma) -
                       0.002697*cos(3*(.data$gamma)) + 0.00148*sin(3*(.data$gamma))),
           cos_wo =
             (sin(-0.8333/360*2*pi) - sin(lat/360*2*pi)*sin(.data$delta/360*2*pi))/
             (cos(lat/360*2*pi)*cos(.data$delta/360*2*pi)),
           sunrise = 12 - acos(.data$cos_wo)/(15/360*2*pi),
           sunset = 12 + acos(.data$cos_wo)/(15/360*2*pi)) |>
    dplyr::select(-.data$gamma, -.data$delta, -.data$cos_wo)
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
#' tmin = c(1, 2, 6), tmax = c(10, 12, 15)) |> add_hourly_temps(lat = 45, hours = 0:23)
#'
#' @importFrom rlang .data
add_hourly_temps <- function(data, lat, hours) {
  data |>
    dplyr::mutate(doy = lubridate::yday(date)) |>
    add_sunrise_sunset(lat = lat) |>
    dplyr::mutate(
      sunrise_prev = dplyr::lag(.data$sunrise),
      sunset_prev = dplyr::lag(.data$sunset),
      sunrise_next = dplyr::lead(.data$sunrise),
      tmin_prev = dplyr::lag(.data$tmin),
      tmax_prev = dplyr::lag(.data$tmax),
      tmin_next = dplyr::lead(.data$tmin)
    ) |>
    tidyr::expand_grid(hour = hours) |>
    dplyr::mutate(
      temp = hourly_temp(
        .data$hour,
        .data$tmin,
        .data$tmax,
        .data$tmin_prev,
        .data$tmax_prev,
        .data$tmin_next,
        .data$sunrise,
        .data$sunset,
        .data$sunrise_prev,
        .data$sunset_prev,
        .data$sunrise_next
      )
    ) |>
    dplyr::select(-.data$sunrise, -.data$sunset, -.data$sunrise_prev,
                  -.data$sunset_prev, -.data$sunrise_next, -.data$tmin_prev,
                  -.data$tmax_prev, -.data$tmin_next
    )
}
