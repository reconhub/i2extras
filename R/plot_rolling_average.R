#' Plot a rolling average incidence object
#'
#' @param x An `incidence2_ra` object created by [add_rolling_average()].
#' @param ... Additional arguments to be passed to
#'   [incidence2::plot.incidence2()] or [incidence2::facet_plot()].
#'
#' @return An incidence plot with the addition of a rolling average.  This will
#'   be facetted if the object is grouped.
#'
#' @importFrom rlang sym
#' @export
plot.incidence2_rolling <- function(x, ...) {

  ra <- attr(x, "rolling_average")
  group_vars <- attr(x, "groups")
  date_var <- attr(x, "date")
  count_var <- attr(x, "count")
  interval <- attr(x, "interval")
  cumulative <- attr(x, "cumulative")
  date_group <- attr(x, "date_group")

  dat <- tidyr::unnest(x, cols = !!sym(ra))

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

  graph +
    ggplot2::geom_point(mapping = ggplot2::aes(x = !!sym(date_var) + shift,
                                               y = !!sym(ra)),
                        position = ggplot2::position_stack(),
                        size = 0.5) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = !!sym(date_var) + shift,
                                              y = !!sym(ra)),
                       position = ggplot2::position_stack())
}
