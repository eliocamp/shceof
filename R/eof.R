#' Flip sign of eofs
#'
#' @param  eof eof object returned from [metR::EOF()]
#' @param n vector of eofs to flip. If `NULL`, defaults to all.
#'
#'
#' @export
flip_ceof <- function(eof, n = NULL) {
  eof <- data.table::copy(eof)

  pc_col <- attr(eof, "suffix", TRUE)
  val_col <- attr(eof, "value.var", TRUE)

  if (is.null(n)) {
    n <- eof$sdev[[pc_col]]
  } else {
    n <- paste0(pc_col, n)
  }

  eof$left[get(pc_col) %in% n, c(val_col) := -get(val_col)]
  eof$right[get(pc_col) %in% n, c(val_col) := -get(val_col)]

  return(eof)
}
