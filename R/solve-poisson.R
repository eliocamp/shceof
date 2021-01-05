#' @useDynLib shceof hwsssp_
#' @export
solve_poisson <- function(forcing, lon, lat) {
  data <- data.table::data.table(forcing = forcing,
                                 lon = lon,
                                 lat = lat)

  forcing <- metR:::.tidy2matrix(data, lat ~ lon, value.var = "forcing")
  pi <- 4*atan(1)

  f <- forcing$matrix

  colat <- (90 - forcing$rowdims$lat)*pi/180
  lon <- forcing$coldims$lon*pi/180

  M <- length(colat) - 1L
  N <- length(lon) - 1L

  TS <- as.single(min(colat))
  TF <- as.single(max(colat))

  TF_is_pi <- isTRUE(all.equal(as.single(pi), TF, check.attributes = FALSE))
  TS_is_zero <- isTRUE(all.equal(0, TS, check.attributes = FALSE))

  if (TF_is_pi & TS_is_zero) {
    # Unspecified boudnary conditions
    MBDCND <- 9L
  } else if (TF_is_pi & !TS_is_zero) {
    # BC unspecified at TF and derivative specified at TS
    MBDCND <- 8L
  } else if (!TF_is_pi & TS_is_zero) {
    # BC unspecified at TS and derivative specified at TF
    MBDCND <- 6L
  } else {
    # Derivative of the solution specifided at TS and TF
    MBDCND <- 3L
  }

  # Derivatives at the poles
  BDTS <- as.single(rep(0., N+1))
  BDTF <- as.single(rep(0., N+1))

  PS <- as.single(min(lon))
  PF <- as.single(max(lon))

  # Periodic boundary condition
  NBDCND <- 0L

  BDPS <- 1L  # dummy value
  BDPF <- 1L  # dummy value

  # lalbda = 0 to get poisson equation
  ELMBDA <- as.single(0.)

  f <- as.single(f)


  IDIMF <- M+1L

  # Working memory
  W <- rep(0., 4*(N+1) + (16 + (ceiling(log2(N+1))))*(M+1))

  PERTRB <- 1L
  IERROR <- 0L


  result <- .Fortran("hwsssp_",
                     TS,
                     TF, M,MBDCND,BDTS,BDTF,
                     PS,PF,N,NBDCND,BDPS,BDPF,ELMBDA,
                     F = f,IDIMF,PERTRB,IERROR= IERROR,W)


  if (result$IERROR != 0) {
    warning("error ", result$IERROR)
    return(NULL)
  }

  forcing$matrix[, ] <- result[["F"]]
  data.table::CJ(lon = forcing$coldims$lon,
                 lat = forcing$rowdims$lat,
                 sorted = FALSE)[, value := c(forcing$matrix)][]

}

