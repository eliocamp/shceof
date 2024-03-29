% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kableExtra.R
\name{column_spec2}
\alias{column_spec2}
\alias{kbl2}
\title{Helpers for kableExtra}
\usage{
column_spec2(kable_input, columns, ...)

kbl2(data, ...)
}
\arguments{
\item{kable_input}{a kable object created by [shceof::kbl()] (not [kableExtra::kbl()]).}

\item{columns}{a vector indicating which column(s) to be selected.}

\item{...}{other arguments to [kableExtra::column_spec] with support for
special symbols. See Details.


`spec_color2` works the same as [kableExtra::spec_color()] with the difference
that the `option` argument can be a palette function returned by
`grDevices::colorRampPalette()`.

For `column_spec2()`, the arguments in `...` can use two
especial symbols. `.table` will refer to the original data.frame object,
and `.col` will refer to each column. The arguments will be evaluated once per
column. This all means that if you have a function `colour_function` that takes
numeric argument and returns assigns a colour to each element, then
`... %>% column_spec2(1:2, colour = colour_function(.table[[.col]])` will
colourise the first and second column.

For `column_spec2()` to work, the table must be generated with `kbl2()`, not
`kableExtra::kbl()`. Also bear in mind that some kableExtra operations will remove
the formating necesary for it to work, so just to be safe, use `column_spec2()` right after `kbl2()`.}

\item{data}{a data.frame-like object.}
}
\description{
Replacements for kableExtra functions that make it easier to directly pipe
resutls
}
