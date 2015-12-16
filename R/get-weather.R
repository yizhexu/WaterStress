get_weather <- function(token, strquery) {
  check_token(token)
  check_query(strquery)

  if (length(strquery) == 1) {
    send_query(token, strquery)
  } else {

    registerDoParallel(detectCores())

    foreach(i = 1:length(strquery), .combine = 'rbind', .export = "send_query") %dopar%
        send_query(token, strquery[i])

  }

}

