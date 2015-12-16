send_query <- function(token, strquery) {

  url_data <- "https://api.awhere.com/v1/weather"

  request <- httr::GET(url_data, query = strquery, httr::add_headers(Authorization = token))

  if ( request$status != 200 ) {
    cat(paste("API call failed. \nStatus", request$status))
  } else {
    # cat(paste("API call success! \n"))
    jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)

  }
}

