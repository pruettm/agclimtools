## code to prepare `maca_2020_cdl` dataset goes here
library(tidyverse)
library(exactextractr)
library(raster)
library(sf)

# read 2020 CDL
cdl <- raster("2020_30m_cdls/2020_30m_cdls.img")

# read VIC grid shapefile
vic <- read_sf("CONUS_VIC/") |>
  st_transform(crs(cdl)) |>
  select(LAT, LON)

# Extract cdl codes
df <- exact_extract(cdl, vic, fun = count_class, append_cols = TRUE) |>
  mutate(location = paste("data", LAT, LON, sep = "_")) |>
  select(location, cdl_code_2020 = Var1, n = Freq) |>
  select(-n)

missing_maca <- read_csv("missing_files.csv")

# remove locations missing from maca data set
maca_2020_cdl <-  df |> filter(!location %in% c(missing$location))

usethis::use_data(maca_2020_cdl, overwrite = TRUE)
