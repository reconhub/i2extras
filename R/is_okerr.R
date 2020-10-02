#' Error handling for incidence2_fit objects
#' 
#' These functions are used to filter succesful model fits from those that 
#' errored.
#'
#' @param x The output of function [fit_curve.incidence2()].
#'
#' @param ... Not currently used.
#' 
#' @details The following accessors are available:
#'
#' * `ok()`: returns rows from an [`incidence2_fit`] object that did not error
#'   during the model fitting stages.
#'
#' * `err()`: returns rows from an [`incidence2_fit`] object that errored
#'   during the model fitting stages.
#'
#' @name is_okerr
NULL

#' @rdname is_okerr
#' @aliases is_ok
#' @export
is_ok <- function(x, ...) {
  UseMethod("is_ok")
}


#' @rdname is_okerr
#' @aliases is_ok.incidence2_fit
#' @export
is_ok.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname is_okerr
#' @aliases is_ok.incidence2_fit
#' @export
is_ok.incidence2_fit <- function(x, ...) {
  error_vars <- attr(x, "error_vars")
  if (!is.null(error_vars)) {
      ok <- suppressMessages(
          lapply(
            x[error_vars],
            function(z) vapply(z, is.null, logical(1))
          )
      )
      
      ok <- do.call(`|`, ok)
      x <- x[ok, ]
  }
  x
}


#' @rdname is_okerr
#' @aliases is_err
#' @export
is_err <- function(x, ...) {
  UseMethod("is_err")
}


#' @rdname is_okerr
#' @aliases is_err.default
#' @export
is_err.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname is_okerr
#' @aliases is_err.incidence2_fit
#' @export
is_err.incidence2_fit <- function(x, ...) {
  error_vars <- attr(x, "error_vars")
  if (!is.null(error_vars)) {
      ok <- suppressMessages(
          lapply(
            x[error_vars],
            function(z) vapply(z, is.null, logical(1))
          )
      )
      ok <- do.call(`|`, ok)
      x <- x[!ok, ]
  }
  x
}