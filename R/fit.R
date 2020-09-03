#' Fit a regression model
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
#'
#' @rdname fit
#' @export
fit <- function(dat, model, ...) {
  UseMethod("fit")
}

#' @rdname fit
#' @aliases fit.default
#' @export
fit.default <- function(dat, model, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(dat), collapse = ", ")))
}


#' @rdname fit
#' @aliases fit.incidence2
#' @export
fit.incidence2 <- function(dat, model = c("negbin", "poisson"), alpha = 0.05, ...) {

  ellipsis::check_dots_empty()
  model <- match.arg(model)
  groups <- incidence2::get_group_names(dat)
  dates <- incidence2::get_dates_name(dat)
  count <- incidence2::get_counts_name(dat)
  fmla <- stats::as.formula(paste(count, "~", dates))

  if (!is.null(groups)) {
    out <- dplyr::nest_by(grouped_df(dat, groups))
    out <- dplyr::mutate(
      out,
      model = list(
        switch(
          model,
          negbin = MASS::glm.nb(fmla, data = .data$data),
          poisson = stats::glm(fmla, data = .data$data, family = stats::poisson),
          stop('Invalid model. Please use one of "negbin" or "poisson".')
        )),
      fitted = list(
        fitted_values(.data$data, model, alpha)
      ),
      r = model$coefficients[2],
      `r-lower` = suppressMessages(stats::confint(model, 2)[1]),
      `r-upper` = suppressMessages(stats::confint(model, 2)[2])
    )

    out$data <- NULL
  } else {
    out <- dat
    out$model <- list(switch(
      model,
      negbin = MASS::glm.nb(fmla, data = dat),
      poisson = stats::glm(fmla, data = dat, family = stats::poisson),
      stop('Invalid model. Please use one of "negbin" or "poisson".')))
    out <- fitted_values(out, out$model[[1]], alpha)
    out <- dplyr::nest_by(out, .data$model, .key = "fitted")
  }

  # create subclass of tibble
  out <- tibble::new_tibble(out,
                            groups = groups,
                            date = dates,
                            count = count,
                            interval = incidence2::get_interval(dat),
                            cumulative = attr(dat, "cumulative"),
                            model = "model",
                            fitted = "fitted",
                            nrow = nrow(out),
                            class = "incidence2_fit")

  attr(out, "date_group") <- attr(dat, "date_group")
  tibble::validate_tibble(out)
}

fitted_values <- function(x, fit, alpha) {
  inverse_link <- stats::family(fit)$linkinv
  pred <- stats::predict(fit, se.fit = TRUE)

  x$fit <- inverse_link(pred$fit)
  x$lower <- inverse_link(pred$fit + stats::qnorm(alpha/2) * pred$se.fit)
  x$upper <- inverse_link(pred$fit + stats::qnorm(1 - alpha/2) * pred$se.fit)
  x
}

#' @export
add_doubling <- function(x) {
  x$doubling <- log(2) / x$r
  x$`doubling-lower` <- log(2) / x$`r-lower`
  x$`doubling-upper` <- log(2) / x$`r-upper`
  x
}
