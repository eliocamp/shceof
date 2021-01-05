#' Partial correlation between x1 and y and x2 and y.
#'
#' @param y variable to which partial correlation will be computed
#' @param x1,x2 variables that will be correlated with y
# #' @param weights vector of weights
#'
#' @export
partial_cor <- function(y, x1, x2) {


  estimate1 <- ppcor::pcor.test(y, x1, x2)
  estimate2 <- ppcor::pcor.test(y, x2, x1)


  # rho_x1x2 <- stats::cov.wt(cbind(x1, x2), wt = weights, cor = TRUE)$cor[1, 2]
  # rho_x1y <- stats::cov.wt(cbind(x1, y), wt = weights, cor = TRUE)$cor[1, 2]
  # rho_x2y <- stats::cov.wt(cbind(x2, y), wt = weights, cor = TRUE)$cor[1, 2]
  #
  # estimate1 <- (rho_x1y - rho_x2y*rho_x1x2 ) / (sqrt(1 - rho_x2y^2) * sqrt(1 - rho_x1x2^2))
  # estimate2 <- (rho_x2y - rho_x1y*rho_x1x2 ) / (sqrt(1 - rho_x1y^2) * sqrt(1 - rho_x1x2^2))

  list(term = c(deparse(substitute(x1)),
                deparse(substitute(x2))),
       partial_correlation = c(estimate1$estimate, estimate2$estimate),
       p.value = c(estimate1$p.value, estimate2$p.value)
  )
}

