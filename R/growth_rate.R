#' Calculate growth rate and doubling time
#' 
#' @param x The output of function [fit_curve.incidence2()].
#' @param alpha Value of alpha used to calculate confidence intervals; defaults
#'   to 0.05 which corresponds to a 95% confidence interval.
#'
#' @param ... Not currently used.
#'
#' @name growth_rate
#' @export
growth_rate <- function(x, ...) {
  UseMethod("growth_rate")
}


#' @rdname growth_rate
#' @aliases growth_rate.default
#' @export
growth_rate.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname growth_rate
#' @aliases growth_rate.incidence2_fit
#' @export
growth_rate.incidence2_fit <- function(x, alpha = 0.05, ...) {
  dat <- ok(x)
  model_var <- attr(dat, "model")
  r <- purrr::map_dbl(dat[[model_var]], ~.$coefficients[2])
  r_lower <- purrr::map_dbl(
    dat[[model_var]], 
    ~suppressMessages(stats::confint(., 2, 1 - alpha)[1])
  )
  r_upper <- purrr::map_dbl(
    dat[[model_var]], 
    ~suppressMessages(stats::confint(., 2, 1 - alpha)[2])
  )
  doubling = log(2) / r
  doubling_lower = log(2) / r_upper
  doubling_upper = log(2) / r_lower

  groups <- attr(dat, "groups")
  if (!is.null(groups)) {
    groups <- dat[[groups]]
  } else {
    groups <- NA_character_
  }

  tibble::tibble(
    groups,
    r,
    r_lower,
    r_upper, 
    doubling,
    doubling_lower,
    doubling_upper)
}