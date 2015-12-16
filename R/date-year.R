date_range <- function(start_date, end_date) {

  start <- as.Date(start_date, "%Y-%m-%d")
  end <-  as.Date(end_date, "%Y-%m-%d")

  loop <- c( 1: (year(end) - year(start)))

  cbind(startDate = as.character(c(start, start + days(365) * loop + 1)),
       endDate = as.character(c(start + days(365) * (loop[1:( length(loop))]), end)))
}
