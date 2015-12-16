#' Check if date input is ISO-8601 formatted: YYYY-MM-DD
#'
#' @param date A date that is ISO-8601 formatted: YYYY-MM-DD.
#' @examples
#' check_date("2015-5-15")
#' check_date("42139")
check_date <- function(date) {
  if ( is.na(as.Date("2015-05-15", "%Y-%m-%d")) ) {
    stop("Date has to be ISO-8601 formatted date in format: YYYY-MM-DD")
  }
}

#' Check if API token exists or valid
#'
#' @param token The API token from \code{get_token()} function.
#' @examples
#' check_token(token)
check_token <- function(token) {
  # parameter validation
  if ( missing(token) | "expire_time" %in% ls()) {
    stop("All regular API requests require a token. Use get_token() to get one")
  } else if (expire_time < Sys.time())  {
    stop("All API tokens expire after an hour. Use get_token() to get one")
  }
}

#' Check if coordinate input is decimall formatted number
#'
#' @param coordinate A decimal-formatted number.
#' @examples
#' check_location(latitude)
check_location <- function(coordinate) {
  if ( is.numeric(coordinate) == FALSE ) {
    stop("Coordinate has to be a decimal-formatted number")
  }
}

#' Check if query string is created by \code{create_query()} function
#'
#' @param strquery The structured query string created by \code{create_query()}
#' function
#' @examples
#' check_query(query)
check_query <- function(strquery) {
  if ( class(strquery) != "query" ) {
    stop("Invalid query string. The query string has to be created by create_query function")
  }
}
