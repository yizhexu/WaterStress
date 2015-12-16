get_attribute <- function(type = NULL) {

  # list of all available attributes
  attribute <- c('minTemperature','maxTemperature','precip','accPrecip','accPrecipPriorYear','accPrecip3YearAverage','accPrecipLongTermAverage','solar', 'minHumidity','maxHumidity','mornWind','maxWind','gdd','accGdd','accGddPriorYear','accGdd3YearAverage','accGddLongTermAverage','pet','accPet','ppet')

  type <- if (is.null(type)) {
    "all"
  } else {
    ifelse(nchar(type) > 4, substr(type, 1, 4), type)
  }

  match <- grep(paste(type, sep = "", collapse = "+"), tolower(attribute))

  if ("all" %in% type) {
    attribute
  } else if (length(match) == 0) {
    NULL
  } else {
    attribute[match]
  }

}

set_date <- function(start_date, end_date = NULL, plant_date = NULL) {

  if(missing(start_date)) stop("Start date is a required input")

  end_date <- if(!is.null(end_date)) {
    check_date(end_date)
    as.character(end_date)
  } else {
    as.character(as.Date(start_date, "%Y-%m-%d") + 14)

  }

  plant_date <- if(!is.null(plant_date)) {
    check_date(plant_date)
    as.character(plant_date)
  } else {
    NULL
  }

  if(!is.null(end_date) & as.Date(end_date, "%Y-%m-%d") - as.Date(start_date, "%Y-%m-%d") > 366) {
    cbind(date_range(start_date, end_date), plantDate = plant_date)
  } else {
    cbind(startDate = start_date, endDate = end_date, plantDate = plant_date)
  }

}

set_grid <- function(latitude, longitude, x, y, size = NULL) {

  if(is.null(size)) size <- 5
  grid <- get_grid(latitude, longitude, x, y, size)

  outer(1:x, 1:y, Vectorize(function(x, y) {
    paste(c("latitude", "longitude"),
          c(grid$latitudes[x], grid$longitudes[y]),
          sep = "=", collapse = "&")
  }))
}

set_gdd <- function(temperature_unit = c("celsius", "fahrenheit"),
                    gdd_method = c(NULL, "min-temp", "modifiedstandard", "min-cap"),
                    base_temp = NULL, max_tempcap = NULL, min_tempcap = NULL) {

  temperature_unit <- if(missing(temperature_unit)) {
    NULL
  } else if (temperature_unit == "celsius") {
    NULL
  } else {
    "fahrenheit"
  }

  gdd_method <- ifelse(missing(gdd_method), "min-temp", gdd_method)

  c(temperatureUnits = temperature_unit, gddMethod = gdd_method, baseTemp = base_temp, maxTempCap = max_tempcap, minTempCap = min_tempcap)
}
