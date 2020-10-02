minimal_incidence <- function(x, groups, date, count, interval,
                              cumulative = FALSE, date_group = NULL) {
  tibble::new_tibble(x,
                     groups = groups,
                     date = date,
                     count = count,
                     interval = interval,
                     cumulative = cumulative,
                     date_group = NULL,
                     nrow = nrow(x),
                     class = "incidence2")
}


base_transpose <- function(l) {
  lapply(seq_along(l[[1]]), function(x) lapply(l, "[[", x))
}


safely <- function(fun) {
    function(...) {
        warn <- err <- NULL
        res <- withCallingHandlers(
            tryCatch(fun(...), error=function(e) {
                err <<- conditionMessage(e)
                NULL
            }), warning=function(w) {
                warn <<- append(warn, conditionMessage(w))
                invokeRestart("muffleWarning")
            })
        list(res, warn=warn, err=err)
    }
}
