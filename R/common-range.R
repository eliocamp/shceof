#' @export
common_range <- function(x, groups) {
  group_order <- grouping(groups)
  group_bounds <- c(0, attr(group_order, "ends"))

  x_ordered <- x[group_order]
  common_range <- c(-Inf, Inf)
  for (i in seq_along(group_bounds)[-1]) {
    new_range <- range(x_ordered[(group_bounds[i-1] + 1):group_bounds[i]])
    common_range <- c(max(common_range[1], new_range[1]),
                      min(common_range[2], new_range[2]))

  }
  common_range
}
