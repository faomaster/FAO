#' Condense String
#'
#'\code{condense_str} shortens a string to a specified length. View details for explanation.
#'
#'\code{condense_str} will not cut off a string midword. If your string's nth character (where n = cutoff)  lies in the middle of a word,
#'\code{condense_str} looks for the end of that word then cuts off the string there.
#'If strict is set to TRUE, \code{condense_str} will always return a string where the number of characters
#'is less than or equal to cutoff.
#'
#' @param x A string.
#' @param cutoff Cutoff index.
#' @param strict Return string with nchar less than or equal to cutoff index?
#' @param rm.spaces Remove all spaces in string after condensing?
#' @param rm.words A vector of characters or words to be removed from \code{x}. All instances removed. Case sensitive.
#'
#' @return A condensed version of \code{x}.
#' @export
#'
#' @examples
#' condense_str("This is a test string", cutoff = 13)
#' condense_str("This is a test string", cutoff = 13, strict = T, rm.spaces = T)
#'
#' condense_str("This is a test string", cutoff = 13, strict = T, rm.words = c("This","test"))

condense_str <- function(x,
                         cutoff = 30,
                         strict = FALSE,
                         rm.spaces = FALSE,
                         rm.words = NULL){

  output <- NULL
  x <- stringr::str_trim(x)

  if(!is.null(rm.words)){
    for(i in 1:length(rm.words)) x <- gsub(rm.words[i], "", x)
    x <- stringr::str_squish(x)
  }

  if(nchar(x) <= cutoff){
    output <- x
  } else {
    cutoff_val <- stringr::str_sub(x, cutoff, cutoff)
    if(cutoff_val != " " && strict == F){
      for(j in cutoff:nchar(x)){
        if(stringr::str_sub(x, j, j) == " " || j == nchar(x)){
          output <- x %>%
            stringr::str_sub(1, j) %>%
            stringr::str_trim()
          break
        }
      }
    } else if (cutoff_val != " " && strict == T) {
      for(k in cutoff:1){
        if(stringr::str_sub(x, k, k) == " "){
          output <- stringr::str_sub(x, 1, k - 1)
          break
        }
      }
    } else {
      output <- stringr::str_sub(x, 1, cutoff - 1)
    }
  }
  if(rm.spaces == T) output <- gsub("[[:space:]]", "", output)
  return(output)
}
