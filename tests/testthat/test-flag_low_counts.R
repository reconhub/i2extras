library(incidence2)
library(outbreaks)

context("flag_low_counts")

test_that("flag_low_counts works as expected", {
  skip_on_cran()

  dat <- dplyr::filter(ebola_sim_clean$linelist,
                date_of_onset >= as.Date("2014-09-01"),
                date_of_onset < as.Date("2015-01-01"))

  target_hosp <- levels(dat$hospital)[1:2]
  target_date <- as.Date("2014-09-29")
  
  i <- incidence(dat,
                 date_index = date_of_onset,
                 interval = 7,
                 group = "hospital",
                 na_as_group = FALSE)
  i <- dplyr::mutate(i, count = dplyr::if_else(
                                           as.Date(date_index) ==  target_date &
                                           hospital %in% target_hosp,
                                           0L,
                                           count))
  

  ## check with set_missing TRUE
  res <- flag_low_counts(i, set_missing = TRUE)
  expect_equal(sum(is.na(res$count)), length(target_hosp))
  expect_true(all(as.Date(i$date_index[is.na(res$count)]) == target_date))
  expect_true(all(i$hospital[is.na(res$count)] %in% target_hosp))

  ## check with set_missing FALSE
  expect_identical(
      is.na(flag_low_counts(i, set_missing = TRUE)$count),
      flag_low_counts(i, set_missing = FALSE)$count_flag_low
  )

})


test_that("flag_low_counts errors as it should", {
  msg <- "`letters` is not an incidence object"
  expect_error(flag_low_counts(letters), msg)
})
