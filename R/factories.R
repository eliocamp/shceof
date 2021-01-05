#' Function factories for adding preffixes or Suffixes
#'
#' @param suffix,preffix character that will be pasted pefore or after the string
#'
#' @export
AddSuffix <- function(suffix = "") {
  function(string) {
    paste0(string, suffix)
  }
}

#' @export
#' @rdname AddSuffix
AddPreffix <- function(preffix = "") {
  function(string) {
    paste0(preffix, string)
  }
}
