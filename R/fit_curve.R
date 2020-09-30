#' Fit an epi curve
#'
#' @param dat An [incidence2::incidence] object.
#' @param model The regression model to fit (can be "negbin" or "poisson").
#' @param alpha Value of alpha used to calculate confidence intervals; defaults
#'   to 0.05 which corresponds to a 95% confidence interval.
#' @param ... Additional arguments to pass to [stats::glm()] for
#'   `model = "poisson"` or [MASS::glm.nb()] for `model = "negbin"`.
#'
#' @return An object of class `incidence2_fit`.
#'
#' @export
fit_curve <- function(dat, model, ...) {
  UseMethod("fit_curve")
}

#' @rdname fit_curve
#' @aliases fit_curve.default
#' @export
fit_curve.default <- function(dat, model, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(dat), collapse = ", ")))
}


#' @rdname fit_curve
#' @aliases fit_curve.incidence2
#' @export
fit_curve.incidence2 <- function(dat,
                                 model = c("negbin", "poisson"),
                                 alpha = 0.05,
                                 ...) {
  ellipsis::check_dots_empty()
  model <- match.arg(model)
  groups <- incidence2::get_group_names(dat)
  dates <- incidence2::get_dates_name(dat)
  count <- incidence2::get_counts_name(dat)
  fmla <- stats::as.formula(paste(count, "~", dates))
  trending_model <- switch(
    model,
    negbin = trending::glm_nb_model(fmla),
    poisson = trending::glm_model(fmla, family = poisson),
    stop('Invalid model. Please use one of "negbin" or "poisson".')
  )

  if (!is.null(groups)) {
    out <- dplyr::nest_by(grouped_df(dat, groups))
    fiterr <- purrr::map(
      out$data,
      function(x) purrr::safely(trending::fit)(trending_model, x)
    )
    fiterr <- purrr::transpose(fiterr)
    prederr <- purrr::map(
      fiterr$result,
      function(x) purrr::safely(predict)(x, interval = "ci")
    )
    prederr <- purrr::transpose(prederr)
    model <- purrr::map(
      fiterr$result,
      function(x) purrr::safely(trending::get_model)(x)
    )
    model <- purrr::transpose(model)
    out$model <- model$result
    out$fitted_values <-prederr$result 
    out$fitted_error <- fiterr$error
    out$predicted_error <-prederr$error
    out$data <- NULL
    error_vars <- c("fitted_error", "predicted_error")


  } else {
    fitted_model <- trending::fit(trending_model, data = dat)
    model <- trending::get_model(fitted_model)
    fitted_values <- predict(fitted_model, interval = "ci")
    out <- tibble(
      model = list(model),
      fitted_values = list(fitted_values)
    )
    error_vars <- NULL
  }

  # create subclass of tibble
  out <- tibble::new_tibble(out,
                            groups = groups,
                            date = dates,
                            count = count,
                            interval = incidence2::get_interval(dat),
                            cumulative = attr(dat, "cumulative"),
                            model = "model",
                            fitted = "fitted_values",
                            error_vars = error_vars,
                            nrow = nrow(out),
                            class = "incidence2_fit")

  attr(out, "date_group") <- attr(dat, "date_group")
  tibble::validate_tibble(out)
}


