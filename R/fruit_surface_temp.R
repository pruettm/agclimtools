#' Calculate fruit surface temperature
#'
#' @param t_air Air temperature (°C)
#' @param wind_speed Wind speed (m/s)
#' @param s_rad Solar radiation (W/m^2)
#' @param t_dew Dew point temperature (°C)
#' @param t_ground Ground temperature (°C), defaults to air temperature
#' @param ground_emissivity Ground emissivity, default 0.97
#' @param fruit_diameter Fruit diameter (m), default 0.08 meters
#' @param fruit_emissivity Fruit emissivity, default 0.95
#' @param fruit_reflectance Fruit reflectance, default 0.6
#' @param fruit_sunlit_prop Proportion of fruit area exposed to sunlight,
#' default 1 for fully exposed fruit
#' @param fruit_groundlit_prop Proportion of fruit area exposed to longwave
#' radiation emitted from ground, default 0 for ground shaded fruit
#' @param fruit_surface_conductance Fruit surface conductance to water vapor
#' diffusion (m/s), default 5E-5 m/s
#' @param print_all_vars Prints all energy balance components if TRUE, default FALSE
#' @param possibly if TRUE returns NA when energy balance can't be solved
#' likely due to invalid coefficients, useful for debugging.
#' Default FALSE
#'
#' @return Fruit surface temperature (°C)
#' @export
#'
#' @examples fruit_surface_temp(t_air = 33, wind_speed = 1,
#' s_rad = 800, t_dew = 30)
fruit_surface_temp <-
  function(t_air,
           wind_speed,
           s_rad,
           t_dew,
           t_ground = t_air,
           ground_emissivity = 0.97,
           fruit_diameter = 0.08,
           fruit_emissivity = 0.95,
           fruit_reflectance = 0.6,
           fruit_sunlit_prop = 1,
           fruit_groundlit_prop = 0,
           fruit_surface_conductance = 5E-5,
           print_all_vars = FALSE,
           possibly = TRUE){

    # Constants #####
    stefan_boltzmann <- 5.67E-8 #Wm^-2T^-4
    specific_heat_air <- 29.3 #Jmol^-1C^-1
    latent_heat_of_vaporisation <- 2.429E6 #J/Kg

    # Boundary Conductance
    g_a <- 1.4*.135*sqrt(wind_speed/(0.84*fruit_diameter))
    delta <- (2503*exp((17.27*t_air)/(t_air + 237.3)))/((t_air + 237.3)^2)
    air_density_over_pressure <- 3.486*(1/(1.01*(t_air + 273)))
    vapour_pressure_air <- 0.6108*exp((17.27*t_air)/(t_air +237.3))
    vapour_pressure_dew <- 0.6108*exp((17.27*t_dew)/(t_dew +237.3))
    atmospheric_emissivity <- 1.24*(((vapour_pressure_dew*10)/(t_air+273))^(1/7))

    # Radiation
    shortwave <- fruit_sunlit_prop*(1-fruit_reflectance)*s_rad
    longwave <- atmospheric_emissivity*stefan_boltzmann*(t_air + 273)^4 +
      fruit_groundlit_prop*ground_emissivity*stefan_boltzmann*(t_ground + 273)^4

    incoming_radiation <- longwave + shortwave

    denominator <- (specific_heat_air*g_a) +
      (latent_heat_of_vaporisation*
         fruit_surface_conductance*
         air_density_over_pressure*delta)

    # a_0
    a_0_numerator <- incoming_radiation +
      specific_heat_air*g_a*t_air +
      (latent_heat_of_vaporisation*fruit_surface_conductance*
         air_density_over_pressure*delta*t_air) -
      (latent_heat_of_vaporisation*fruit_surface_conductance*
         air_density_over_pressure*(vapour_pressure_air - vapour_pressure_dew))

    a_0 <- -((a_0_numerator/denominator)+273)

    # a_4
    a_4 <- (fruit_emissivity*stefan_boltzmann)/denominator

    if (print_all_vars) {
      cat(paste(
        "\n---------------------------------",
        "\nAir Temp:", round(t_air),
        "\nWind Speed:", round(wind_speed, 2),
        "\nSolar:", round(s_rad),
        "\nDew Point Temp:", t_dew,
        "\nBoundary Conductance:", round(g_a, 2),
        "\nDelta:", round(delta, 2),
        "\nAir Density Over Pressure:", round(air_density_over_pressure, 2),
        "\nVapour Pressure (Air):", round(vapour_pressure_air, 2),
        "\nVapour Pressure (Dew):", round(vapour_pressure_dew, 2),
        "\nAtmospheric Emissivity:", round(atmospheric_emissivity, 2),
        "\nShortwave:", round(shortwave),
        "\nLongwave:", round(longwave),
        "\na_0:", a_0,
        "\na_4:", a_4))
    }

    solve <- function(a_0, a_4){

      sol <- polyroot(c(a_0,1,0,0,a_4))

      # filter solution set to only positive real solution
      sol_real <- Re(sol[abs(Im(sol)) < 1E-5])
      sol_final <- sol_real[sol_real > 0] - 273
      return(sol_final)
    }

    # solve quartic

    if (possibly) {
      sol <- purrr::map2_dbl(a_0, a_4, purrr::possibly(solve, otherwise = NA))
    } else {
      sol <- purrr::map2_dbl(a_0, a_4, solve)
    }

    return(sol)

  }
