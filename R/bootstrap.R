#' Bootstrap incidence time series
#'
#' This function can be used to bootstrap [incidence2::incidence] objects.
#' Bootstrapping is done by sampling with replacement the original input dates.
#' See `details` for more information on how this is implemented.
#'
#' @author Thibaut Jombart, Tim Taylor
#'
#' @details As original data are not stored in [incidence2::incidence] objects,
#'   the bootstrapping is achieved by multinomial sampling of date bins weighted
#'   by their relative incidence.
#'
#' @param x An [incidence2::incidence] object.
#'
#' @param randomise_groups A `logical` indicating whether groups should be
#'   randomised as well in the resampling procedure; respective group sizes will
#'   be preserved, but this can be used to remove any group-specific temporal
#'   dynamics. If `FALSE` (default), data are resampled within groups.
#'
#' @return An `incidence2` object.
#'
#' @seealso [find_peak()] to use estimate peak date using bootstrap
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   data(fluH7N9_china_2013, package = "outbreaks")
#'   i <- incidence2::incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'   bootstrap(i)
#'   bootstrap(i, randomise_groups = TRUE)
#' }
#'
#' @import data.table
#' @export
bootstrap <- function(x, randomise_groups = FALSE) {

  if (!inherits(x, "incidence2")) {
    stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }

  # get relevant column names
  date_var <- incidence2::get_dates_name(x)
  count_vars <- incidence2::get_counts_name(x)
  group_vars <- incidence2::get_group_names(x)

  # calculate bootstrap for each count variable
  res <- lapply(
    count_vars,
    FUN = bootstrap_single_count,
    dat = x,
    date_var = date_var,
    group_vars = group_vars,
    randomise = randomise_groups
  )

  # If there are multiple count variables we need to merge them together
  out <- res[[1]]
  if (length(res) > 1) {
    for (i in 2:length(res)) {
      out <- merge(out, res[[i]], by = c(date_var, group_vars), all = TRUE)
    }
  }

  # TODO - this should really be a function exported from incidence2
  # create incidence2 object
  tbl <- tibble::new_tibble(out,
                            groups = group_vars,
                            date = date_var,
                            counts = count_vars,
                            interval = incidence2::get_interval(x),
                            cumulative = attr(x, "cumulative"),
                            nrow = nrow(out),
                            class = "incidence2"
  )
  tibble::validate_tibble(tbl)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# Bootstrap over a single count column
bootstrap_single_count <- function(count_var, dat, date_var, group_vars, randomise) {

  # drop any additional count columns
  tmp <- as.data.table(dat[, c(date_var, group_vars, count_var)])

  # incidence object may have NA when multiple counts present so we remove
  tmp <- na.omit(tmp, cols = count_var)

  # overwrite (by reference) the count column with the bootstrapped values
  tmp[, (count_var) := rmultinom(1, sum(.SD[[count_var]]), .SD[[count_var]]), .SDcols = count_var]

  # randomise groups (by reference) if desired
  if (randomise && !is.null(group_vars)) {
    tmp[, (group_vars) := lapply(.SD, sample_), .SDcols = group_vars]
  }
  tmp
}


# A fix for the behaviour of `sample` when first argument is of length 1.
sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}
