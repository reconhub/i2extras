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
#' @name okerr
NULL

#' @rdname okerr
#' @aliases ok
#' @export
ok <- function(x, ...) {
  UseMethod("ok")
}


#' @rdname okerr
#' @aliases ok.incidence2_fit
#' @export
ok.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname okerr
#' @aliases ok.incidence2_fit
#' @export
ok.incidence2_fit <- function(x, ...) {
  error_vars <- attr(x, "error_vars")
  if (!is.null(error_vars)) {
      ok <- suppressMessages(
          purrr::map(x[error_vars], function(z) purrr::map_lgl(z, is.null))
      )
      
      ok <- do.call(`|`, ok)
      x <- x[ok, ]
  }
  x
}


#' @rdname okerr
#' @aliases err
#' @export
err <- function(x, ...) {
  UseMethod("err")
}


#' @rdname okerr
#' @aliases err.default
#' @export
err.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname okerr
#' @aliases err.incidence2_fit
#' @export
err.incidence2_fit <- function(x, ...) {
  error_vars <- attr(x, "error_vars")
  if (!is.null(error_vars)) {
      ok <- suppressMessages(
          purrr::map(x[error_vars], function(z) purrr::map_lgl(z, is.null))
      )
      ok <- do.call(`|`, ok)
      x <- x[!ok, ]
  }
  x
}