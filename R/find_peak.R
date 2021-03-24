#' Find the peak date of an incidence curve
#'
#' This function can be used to find the peak of an epidemic curve stored as an
#' [incidence2::incidence] object.
#'
#' @author Tim Taylor
#'
#' @param x An [incidence2::incidence] object.
#'
#' @return A tibble containing the date of the (first) highest incidence in the
#'   data along with the count. If `x` is grouped object then the output will
#'   have the peak calculated for each grouping.
#'
#' @seealso [estimate_peak()] for bootstrap estimates of the peak time.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'
#'   # load data and create incidence
#'   data(fluH7N9_china_2013, package = "outbreaks")
#'   i <- incidence2::incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'   find_peak(i)
#'
#' }
#'
#' @import data.table
#' @export
find_peak <- function(x) {

  if (!inherits(x, "incidence2")) {
    stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }

  # get relevant column names
  date_var <- incidence2::get_dates_name(x)
  count_vars <- incidence2::get_counts_name(x)
  group_vars <- incidence2::get_group_names(x)

  # calculate peak for each count variable
  res <- lapply(
    count_vars,
    FUN = find_peak_single_count,
    dat = x,
    date_var = date_var,
    group_vars = group_vars
  )

  # If there are multiple count variables we need to merge them together
  # and then convert to long format
  out <- res[[1]]
  if (length(res) > 1) {
    for (i in 2:length(res)) {
      out <- merge(out, res[[i]], by = c(date_var, group_vars), all = TRUE)
    }
    out <- melt(
      out,
      id.vars = c(group_vars, date_var),
      measure.vars = count_vars,
      na.rm = TRUE,
      variable.name = "count_variable",
      value.name = "count"
    )
  }

  setorder(out)
  tibble::as_tibble(out)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

find_peak_single_count <- function(count_var, dat, date_var, group_vars) {

  # drop any additional count columns
  res <- as.data.table(dat[, c(date_var, group_vars, count_var)])

  # order in descending order of groups and then counts
  setorderv(res, cols = c(group_vars, count_var), order = -1, na.last = TRUE)

  # If no groups just return the ordered res else the head value for each group
  if (is.null(group_vars)) {
    res[1]
  } else {
    res[, head(.SD, 1), by = group_vars]
  }
}
