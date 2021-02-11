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
#' `spec_color2` works the same as [kableExtra::spec_color()] with the difference
#' that the `option` argument can be a palette function returned by
#' `grDevices::colorRampPalette()`.
#'
#' For `column_spec2()`, the arguments in `...` can use two
#' especial symbols. `.table` will refer to the original data.frame object,
#' and `.col` will refer to each column. The arguments will be evaluated once per
#' column. This all means that if you have a function `colour_function` that takes
#' numeric argument and returns assigns a colour to each element, then
#' `... %>% column_spec2(1:2, colour = colour_function(.table[[.col]])` will
#' colourise the first and second column.
#'
#' For `column_spec2()` to work, the table must be generated with `kbl2()`, not
#' `kableExtra::kbl()`. Also bear in mind that some kableExtra operations will remove
#' the formating necesary for it to work, so just to be safe, use `column_spec2()` right after `kbl2()`.
#'
#' @export
column_spec2 <- function(kable_input, columns, ...) {
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
#' @rdname column_spec2
spec_color2 <- function(x, alpha = 1, begin = 0, end = 1,
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
#' @rdname column_spec2
kbl2 <- function(data, ...) {
  k <- kableExtra::kbl(data, ...)
  attr(k, ".table") <- data
  k
}
