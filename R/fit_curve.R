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
  model <- match.arg(model)
  groups <- incidence2::get_group_names(dat)
  dates <- incidence2::get_dates_name(dat)
  count <- incidence2::get_counts_name(dat)
  fmla <- stats::as.formula(paste(count, "~", dates))
  trending_model <- switch(
    model,
    negbin = trending::glm_nb_model(fmla, ...),
    poisson = trending::glm_model(fmla, family = "poisson", ...),
    stop('Invalid model. Please use one of "negbin" or "poisson".')
  )

  if (!is.null(groups)) {
    out <- dplyr::nest_by(grouped_df(dat, groups))
    fiterr <- lapply(
      out$data,
      function(x) safely(trending::fit)(trending_model, x)
    )
    fiterr <- base_transpose(fiterr)
    prederr <- lapply(
      fiterr[[1]],
      function(x) safely(predict)(x, interval = "ci")
    )
    prederr <- base_transpose(prederr)
    model <- lapply(
      fiterr[[1]],
      function(x) safely(trending::get_model)(x)
    )
    model <- base_transpose(model)
    out$model <- model[[1]]
    out$estimates <-prederr[[1]] 
    out$fitting_warning <- fiterr[[2]]
    out$fitting_error <- fiterr[[3]]
    out$prediction_warning <-prederr[[3]]
    out$prediction_error <-prederr[[3]]
    out$data <- NULL
    warning_vars <- c("fitting_warning", "prediction_warning")
    error_vars <- c("fitting_error", "prediction_error")

  } else {
    fitted_model <- trending::fit(trending_model, data = dat)
    model <- trending::get_model(fitted_model)
    estimates <- predict(fitted_model, interval = "ci")
    out <- tibble(
      model = list(model),
      estimates = list(fitted_values)
    )
    warning_vars <- NULL
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
                            fitted = "estimates",
                            warning_vars = warning_vars,
                            error_vars = error_vars,
                            nrow = nrow(out),
                            class = "incidence2_fit")

  attr(out, "date_group") <- attr(dat, "date_group")
  tibble::validate_tibble(out)
}


