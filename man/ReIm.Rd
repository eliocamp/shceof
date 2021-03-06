% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReIm.R
\name{ReIm}
\alias{ReIm}
\alias{sep_ReIm}
\alias{factor_ReIm}
\title{Splits complex numbers into real and imaginary vectors}
\usage{
ReIm(complex)

sep_ReIm(data, column, format = c("longer", "wider"))

factor_ReIm(part)
}
\arguments{
\item{complex}{vector of complex values}

\item{data}{a data.table}

\item{column}{unquoted name of the column to split into real and imaginary part}

\item{format}{Whether to return a long or wide dataset.}

\item{part}{Character vector with elements "Real" and "Imaginary"}
}
\description{
Splits complex numbers into real and imaginary vectors
}
