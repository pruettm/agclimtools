% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solar.R
\name{extrat}
\alias{extrat}
\title{Extraterrestrial Solar Radiation from Allen et al.}
\usage{
extrat(hour, doy, lat, long, time_zone_long, freq_h = 1)
}
\arguments{
\item{hour}{hour of day}

\item{doy}{day of year}

\item{lat}{latitude}

\item{long}{longitude}

\item{time_zone_long}{longitude of time zone}

\item{freq_h}{frequency of observations in hours defaults to 1 hour (0.5 = half hour)}
}
\value{
Extraterrestrial solar radiation in W/m^2
}
\description{
Extraterrestrial Solar Radiation from Allen et al.
}
\examples{
extrat(0:23, 34, 45, -119, -120)
}
