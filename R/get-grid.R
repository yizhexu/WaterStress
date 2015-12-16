get_line <- function(coordinate, n, size = NULL) {

  if(is.null(size)) size <- 5

  if (n == 1) {
    c(coordinate)
  } else if (n %% 2 == 0) {
    c(coordinate - (n/2):1 * size/60, coordinate + 1:(n/2) * size/60)
  } else {
    c(coordinate - ((n - 1)/2):1 * size/60, coordinate, coordinate + 1:((n - 1)/2) * size/60)
  }
}

get_grid <- function(latitude, longitude, x, y, size = NULL) {
  # this function will provide centroid lat/lng for x*y grids
  if(is.null(size)) size <- 5

  list("latitudes" = get_line(latitude, x, size),
       "longitudes" = get_line(longitude, y, size))
}

