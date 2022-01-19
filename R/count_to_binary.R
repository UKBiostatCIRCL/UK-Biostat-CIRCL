#### Converting Count Dataset to Long Format ####


#' Count to Binary Dataframe
#'
#' This function receives the inputs of a count-based dataframe - typically longitudinal in nature - and
#' converts these counts to binary form where each row represents an independent event resulting in a 0 or 1 outcome.
#' This may be useful in situations of n events, choose k successes.
#' @param dat A dataframe in person-period/long format.
#' @param x A numeric vector representing the total number of events (n).
#' @param y A numeric vector representing the total number of successes (k).
#' @param time A numeric vector representing the time points.
#' @keywords Count to Binary
#' @import dplyr data.table
#' @examples
#' new_dat = to_binary(dat, dat$x, dat$y, dat$time)


count_to_binary = function(dat, x, y, time) {
  new_dat = list()
  for (i in 1:nrow(dat)) {
    new_dat[[i]] =
      data.frame(x = rep(x[i], x[i]),
                 time = rep(time[i], x[i]),
                 y = c(rep(1, y[i]),
                       rep(0, x[i] - y[i])))
  }
  data.table::rbindlist(new_dat)
  new_dat = dplyr::bind_rows(new_dat, .id = "time")
  return(new_dat)
}

