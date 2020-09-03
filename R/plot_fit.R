#' Plot a rolling average incidence object
#'
#' @param x An `incidence2_fit` object created by [fit()].
#' @param ... Additional arguments to be passed to
#'   [incidence2::plot.incidence2()] or [incidence2::facet_plot()].
#'
#' @return An incidence plot with the addition of a fitted curve.  This will
#'   be facetted if the object is grouped.
#'
#' @importFrom rlang sym
#' @export
plot.incidence2_fit <- function(x, ...) {

  group_vars <- attr(x, "groups")
  date_var <- attr(x, "date")
  count_var <- attr(x, "count")
  interval <- attr(x, "interval")
  cumulative <- attr(x, "cumulative")
  date_group <- attr(x, "date_group")

  x$model <- NULL

  dat <- tidyr::unnest(x, tidyr::everything())
  dat <- minimal_incidence(
    dat,
    groups = group_vars,
    date = date_var,
    count = count_var,
    interval = interval,
    cumulative = cumulative,
    date_group = date_group
  )

  if (is.null(group_vars)) {
    graph <- plot(dat, ...)
  } else {
    graph <- incidence2::facet_plot(dat, ...)
  }


  shift <- mean(incidence2::get_interval(dat, integer = TRUE))/2
  col_model <- "#BBB67E"

  graph +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = !!sym(date_var) + shift, y = .data$fit),
      size = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(x = !!sym(date_var) + shift,
                                      ymin = .data$lower,
                                      ymax = .data$upper),
                         alpha = 0.4,
                         fill = col_model)
}
