#' Convert a string to proper format.
#'
#' @param x A string.
#'
#' @return The proper version of \code{x}.
#' @export
#'
#' @examples
#' proper("hello world")
proper <- function(x){
  str <- unlist(strsplit(x, ''))
  ind <- c(0, grep(" ", str)) + 1
  str[ind] <- toupper(str[ind])
  return(paste0(str, collapse = ''))
}
