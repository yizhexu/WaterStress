# library for awhere data
library(httr)
library(jsonlite)
library(magrittr)
library(doParallel)
library(foreach)
library(lubridate)
library(devtools)

# library for shiny
library(leaflet)
library(shiny)
library(markdown)

source("./R/check-input.R")
source("./R/create-query.R")
source("./R/date-year.R")
source("./R/get-grid.R")
source("./R/get-token.R")
source("./R/get-weather.R")
source("./R/send-query.R")
source("./R/set-options.R")


# load("./data/data.RData")
load("./data/baseline.RData")
load("./data/target.RData")

# map icon
aWhereIcon <- makeIcon(
  iconUrl = "http://www.awhere.com/aWhereSite/media/aWhereLibrary/style/images/icon2.png",
  iconWidth = 26.5, iconHeight = 32
)

# function
today <- Sys.Date()
this_year <- year(today)
attribute <- c("accPrecip", "accPet")

calculate_index <- function(start_date, end_date, data) {
  start <- as.Date(data[, 4, drop = TRUE], "%Y-%m-%d") == start_date
  end <- as.Date(data[, 4, drop = TRUE], "%Y-%m-%d") == end_date
  new_data <- data[end == TRUE, 5:6, drop = FALSE] - data[start == TRUE, 5:6, drop = FALSE]
  index <- new_data[, "dailyAttributes.accPrecip", drop = TRUE] / new_data[, "dailyAttributes.accPet", drop = TRUE]
  ifelse(index < 0.4, 0.4, ifelse( index > 1.3, 1.3, round(index, digits = 2) ))
}

calculate_coor <- function(start_date, data) {
  start <- as.Date(data[, 4], "%Y-%m-%d") == start_date
  data[start == TRUE, 2:3, drop = FALSE]
}

create_rectangle <- function(coors, x, size = 5) {
  lat_m <- coors$latitude[x] - size/2/60
  lat_p <- coors$latitude[x] + size/2/60
  lng_m <- coors$longitude[x] - size/2/60
  lng_p <- coors$longitude[x] + size/2/60
  c(lng_m, lat_p, lng_p, lat_m)
}

# palette
pal <- colorNumeric(
  palette = "RdYlBu",
  domain = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3)
)

pal2 <- colorNumeric(
  palette = "RdYlBu",
  domain = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
)