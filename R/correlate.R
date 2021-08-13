#' Compute correlations
#'
#' Thin wrapper around [cor.test] which returns estimate, p.value, low and high
#' 95% confidence interval and a text ready for reporting.
#'
#' @param x,y numeric variables to correlate.
#' @param signif number of significant figures to keep in the reporting text.
#' @param sep separator to use between the estimate and the confidence interval.
#' The default uses a protected space.
#'
#' @export
correlate <- function(x, y, signif = 2, sep = "\ ") {
  correlation <- cor.test(x, y)
  out <- with(correlation,
              list(estimate = estimate,
                   p.value = p.value,
                   low = conf.int[1],
                   hig = conf.int[2]))


  out$text <- with(out,
                   paste0(round(estimate, signif), sep, "(CI:\ ",
                          round(low, signif), "\ --\ ", round(hig, signif), ")"))

  out
}
