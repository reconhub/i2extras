#' Estimate the peak date of an incidence curve using bootstrap
#'
#' This function can be used to estimate the peak of an epidemic curve stored as
#' [incidence2::incidence] object, using bootstrapped samples of the available
#' data. See [`bootstrap()`] for more information on the resampling.
#'
#' @author Thibaut Jombart and Tim Taylor, with inputs on caveats from Michael Höhle.
#'
#' @details Input dates are resampled with replacement to form bootstrapped
#'   datasets; the peak is reported for each, resulting in a distribution of
#'   peak times. When there are ties for peak incidence, only the first date is
#'   reported.
#'
#' Note that the bootstrapping approach used for estimating the peak time makes
#' the following assumptions:
#'
#' - the total number of event is known (no uncertainty on total incidence)
#' - dates with no events (zero incidence) will never be in bootstrapped
#'   datasets
#' - the reporting is assumed to be constant over time, i.e. every case is
#'   equally likely to be reported
#'
#' @param x An [incidence2::incidence] object.
#'
#' @param n The number of bootstrap datasets to be generated; defaults to 100.
#'
#' @param alpha The type 1 error chosen for the confidence interval; defaults to
#'   0.05.
#'
#' @param progress Should a progress bar be displayed (default = TRUE)
#'
#' @return A tibble with the the following columns:
#'
#' - `observed_date`: the date of peak incidence of the original dataset.
#' - `observed_count`: the peak incidence of the original dataset.
#' - `estimated`: the median peak time of the bootstrap datasets.
#' - `lower_ci/upper_ci`: the confidence interval based on bootstrap datasets.
#' - `peaks`: a nested tibble containing the the peak times of the bootstrapped
#'   datasets.
#'
#' @seealso [`bootstrap()`] for the bootstrapping underlying this approach and
#'   [`find_peak()`] to find the peak in a single [incidence2::incidence]
#'   object.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'
#'   # load data and create incidence
#'   data(fluH7N9_china_2013, package = "outbreaks")
#'   i <- incidence2::incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'
#'   # find 95% CI for peak time using bootstrap
#'   peak_data <- estimate_peak(i)
#'   peak_data
#'   summary(peak_data$peaks)
#' }
#'
#' @import data.table
#' @export
estimate_peak <- function(x, n = 100, alpha = 0.05, progress = TRUE) {

  if (!inherits(x, "incidence2")) {
    stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }

  # get relevant column names
  date_var <- incidence2::get_dates_name(x)
  count_vars <- incidence2::get_counts_name(x)
  group_vars <- incidence2::get_group_names(x)

  # Calculate the observed peak and convert to data.table for later
  observed_peak <- find_peak(x)
  setDT(observed_peak)

  # Calculate peaks from bootstrapped data samples with optional progress bar
  if (progress) {
    out <- vector("list", n)
    message("Estimating peaks from bootstrap samples:")
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    for (i in seq_len(n)) {
      out[[i]] <- find_peak(bootstrap(x))
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
    cat("\n")
  } else {
    out <- lapply(seq_len(n), function(y) find_peak(bootstrap(x), regroup = FALSE))
  }
  out <- rbindlist(out)

  # To deal with multiple counts we introduce a dummy variable
  dummy <- NULL
  if (length(count_vars) > 1) {
    dummy <- "count_variable" # hard-coded from find_peak!
  }
  grouping_variables <- c(group_vars, dummy)

  # TODO - check this with Thibaut
  # specify probabilities (lower_ci, median, upper_ci)
  probs <- c(alpha/2, 0.5, 1 - alpha/2)

  # first deal with the case when there is only one count and no groupings
  if (is.null(grouping_variables)) {
    quantiles <- out[, Reduce(c, lapply(.SD, quantiles_list, probs = probs)), .SDcols = date_var]
    quantiles[, bootstrap_peaks := list(tibble::as_tibble(out))]
    quantiles[, observed_peak := ..observed_peak[[date_var]]]
    quantiles[, observed_count := ..observed_peak[[count_vars]]]
    setcolorder(quantiles, c(group_vars, "observed_peak", "observed_count", "bootstrap_peaks"))
    return(tibble::as_tibble(quantiles))
  }

  # If we haven't returned above we have groups and or multiple counts to deal with

  # group peaks by group_vars and (potentially) "count_variable"
  peaks <- out[, .(bootstrap_peaks = list(.SD)), by = grouping_variables]

  # group quantiles by group_vars and (potentially) "count_variable"
  quantiles <- out[, Reduce(c, lapply(.SD, quantiles_list, probs = probs))
                   , .SDcols = date_var
                   , keyby = grouping_variables]

  # join peaks on to quantiles (by reference)
  quantiles[peaks, on = grouping_variables, bootstrap_peaks := i.bootstrap_peaks]

  # case with multiple count variables
  if (length(count_vars) > 1) {
    quantiles[observed_peak
              , on = c(group_vars, dummy)
              , `:=`(observed_peak = observed_peak[[date_var]],
                     observed_count = observed_peak[["count"]])] # hard-coded from find_peak!
    setcolorder(
      quantiles,
      c(group_vars, "count_variable", "observed_peak", "observed_count", "bootstrap_peaks")
    )
  } else {
    #case with one count variable
    quantiles[observed_peak
              , on = c(group_vars)
              , `:=`(observed_peak = observed_peak[[date_var]],
                     observed_count = observed_peak[[count_vars]])]
    setcolorder(quantiles, c(group_vars, "observed_peak", "observed_count", "bootstrap_peaks"))
  }

  # convert nested data.tables to tibbles
  quantiles[, bootstrap_peaks := lapply(bootstrap_peaks, tibble::as_tibble)]

  return(tibble::as_tibble(quantiles))
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

quantiles_list <- function(x, probs) {
  res <- quantile(x, probs = probs, names = FALSE, type = 1)
  res <- setNames(res, c("lower_ci", "median", "upper_ci"))
  as.list(res)
}
