#' @export
invert_laplacian <- function(v, x, y) {
  a <- 6371000
  field <- data.table::data.table(x, y, v)

  field[, c("k", "l", "v.hat") := fft2d(v, x, y)]
  field[, lp := l*2*pi/a]
  field[, kp := k*2*pi/(a*cos(y*pi/180))]

  field[, v.hat := -v.hat/(k^2 + l^2)]
  # field[, v.hat := -v.hat/(kp^2 + lp^2)]
  field[, v.hat := c(0, v.hat[2:.N])]

  field[, c("k", "l", "v.inv") := fft2d(v.hat, k, l, inverse = TRUE)]

  return(Re(field$v.inv))
}

fft2d <- function(v, x, y, inverse = FALSE) {

  field <- as.matrix(reshape2::dcast(data.table::data.table(v, x, y),
                                     y ~ x, value.var = "v"))

  field.fft <- fft(field[, -1], inverse = inverse)

  dimnames(field.fft) <- list(
    k = 1:nrow(field.fft) - 1,
    l = 1:ncol(field.fft) - 1)

  field.fft <- setDT(melt(field.fft, value.name = "f"))

  with(field.fft, list(k, l, f))
}
