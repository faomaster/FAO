#' Remove Invalid Excel Sheet Characters
#'
#'\code{rm_invalid} Removes invalid excel sheet special characters.
#'
#'\code{rm_invalid} is useful if you are writing sheets to excel. Excel will not allow certain special characters in sheet names.
#'Invalid characters include colons, square brackets, asterisks, question marks, vertical bars, new lines and tabs.
#'
#' @param x A string or vector of strings.

#' @return \code{x} with invalid characters removed.
#' @export
#'
#' @examples
#' rm_invalid(c("excel sheet* example[]: a fake string", "another test::"))
#'
#' library(xlsx)
#' string <- "Table* [5]?"
#' df <- data.frame(foo = 1:3, bar = 1:3)
#' sName <- rm_invalid(string)
#' write.xlsx(df, "foo.xlsx", string) #Will throw error
#' write.xlsx(df, "foo.xlsx", sName)  #No error

rm_invalid <- function(x) {
  gsub("\\*|:|\\r|\\n|\\|//|\\?|\\[|\\]","", x)
}
