% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute-ceof.R
\name{compute_ceof}
\alias{compute_ceof}
\title{Computes ceof}
\usage{
compute_ceof(
  hgt,
  lon,
  lat,
  lev,
  temporal = FALSE,
  lats.eof = c(-90, -20),
  n = 1:2
)
}
\arguments{
\item{temporal}{Logical whether to compute temporal anomalies.}

\item{lats.eof}{Range of latitudes.}

\item{n}{Numeric vector of EOFs to compute.}

\item{dt}{data.table.}
}
\description{
Computes ceof
}
