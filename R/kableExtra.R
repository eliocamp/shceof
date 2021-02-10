#' Helpers for kableExtra
#'
#' Replacements for kableExtra functions that make it easier to directly pipe
#' resutls
#'
#' @param kable_input a kable object created by [shceof::kbl()] (not [kableExtra::kbl()]).
#' @param data a data.frame-like object.
#' @param columns a vector indicating which column(s) to be selected.
#' @inheritParams kableExtra::spec_color
#' @param ... other arguments to [kableExtra::column_spec] with support for
#' special symbols. See Details.
#'
#'
#' `spec_color` works the same as [kableExtra::spec_color()] with the difference
#' that the `option` argument can be a palette function returned by
#' `grDevices::colorRampPalette()`.
#'
#' For `columns_spec()` (notice the s), the arguments in `...` can use two
#' especial symbols. `.table` will refer to the original data.frame object,
#' and `.col` will refer to each column. The arguments will be evaluated once per
#' column. This all means that if you have a function `colour_function` that takes
#' numeric argument and returns assigns a colour to each element, then
#' `... %>% columns_spec(1:2, colour = colour_function(.table[[.col]])` will
#' colourise the first and second column.
#'
#' For `columns_spec()` to work, the table must be generated with `shceof::kbl()`, not
#' `kableExtra::kbl()`. Also bear in mind that some kableExtra operations will remove
#' the formating necesary for it to work, so just to be safe, use `columns_spec()` right after `kbl()`.
#'
#' @export
columns_spec <- function(kable_input, columns, ...) {
  a <- match.call(expand.dots = FALSE)

  for (col in columns) {
    dots <- lapply(a$..., function(i) eval(i, envir  = list(.col = col,
                                                            .table = attr(kable_input, ".table"))))
    args <- c(list(kable_input = kable_input, column = col), dots)
    kable_input <- do.call(kableExtra::column_spec, args)
  }

  return(kable_input)
}

#' @export
#' @rdname columns_spec
spec_color <- function(x, alpha = 1, begin = 0, end = 1,
                       direction = 1, option = "D",
                       na_color = "#BBBBBB", scale_from = NULL) {
  if (is.function(option)) {
    palette <- option(256)
  } else {
    palette <- viridisLite::viridis(256, alpha, begin, end, direction, option)
  }


  if (is.null(scale_from)) {
    x <- round(scales::rescale(x, c(1, 256)))
  } else {
    x <- round(scales::rescale(x, to = c(1, 256),
                               from = scale_from))
  }

  color_code <- palette[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}


#' @export
#' @rdname columns_spec
kbl <- function(data, ...) {
  k <- kableExtra::kbl(data, ...)
  attr(k, ".table") <- data
  k
}
