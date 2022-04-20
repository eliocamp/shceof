#' Change dimension names
#'
#' Changes the names of columns in a dataframe according to a set of rules
#'
#' @param data A data.table.
#' @param rules A named list defining renaming rules.
#' @param extra Extra rules that will be concatenates with `rules`.
#' This allows to quickly add extra rules to the default ones.
#'
#' @export
normalise_coords <- function(data,
                             rules =  list(lev = c("level"),
                                           lat = c("latitude"),
                                           lon = c("longitude", "long"),
                                           time = c("date")),
                             extra = list()) {
  rules <- c(rules, extra)

  for (f in seq_along(rules)) {
    old <- colnames(data)[colnames(data) %in% rules[[f]]]

    if (length(old) != 0) {
      data.table::setnames(data,
                           old,
                           names(rules)[[f]], skip_absent = TRUE)
    }
  }
  return(invisible(data))
}
