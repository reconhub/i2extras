minimal_incidence <- function(x, groups, date, count, interval,
                              cumulative = FALSE, date_group = NULL) {
  tibble::new_tibble(x,
                     groups = groups,
                     date = date,
                     count = count,
                     interval = interval,
                     cumulative = cumulative,
                     date_group = NULL,
                     nrow = nrow(x),
                     class = "incidence2")
}


base_transpose <- function(l) {
  lapply(seq_along(l[[1]]), function(x) lapply(l, "[[", x))
}


safely <- function(fun) {
    function(...) {
        warn <- err <- NULL
        res <- withCallingHandlers(
            tryCatch(
                fun(...),
                error = function(e) {
                    err <<- conditionMessage(e)
                    NULL
                }
            ),
            warning = function(w) {
                warn <<- append(warn, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )
        list(res, warn = warn, err = err)
    }
}


#' Generate NAs of the right type for counts
#'
#' Counts can be of type integer or double. When setting up NAs to counts, this
#' needs to be reflected by using the right type of NAs. This function addresses
#' this need.
#'
#' @author Thibaut
#'
#' @param x A count vector.
#'
#' @return A `NA` of the type matching the input.
#' 
NA_counts_ <- function(x) {
  if (is.integer(x)) {
    return(NA_integer_)
  } else if (is.double(x)) {
    return(NA_real_)
  } else {
    msg <- sprintf(
        "Cannot set NA values for counts of type `%s`",
        typeof(x))
    stop(msg)
  }
}
