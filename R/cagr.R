#' Compound Average Growth Rate
#'
#'\code{cagr} calculates compound average growth rate(s) for the vector passed as an argument.
#'
#'If matrix is equal to TRUE then \code{cagr} will return a matrix of calculated cagrs between each numerical value
#'within the vector passed as an argument. If time is NULL, column names will equal to the base value index and row names will
#'equal to the final value index. For example, in the return matrix, if column name = 2 and row name = 5, the
#'resulting value will equal to the cagr between the second and fifth value in the vector \code{x}. View examples for more detail.
#'
#' @param x A vector of numerical values.
#' @param matrix Return a matrix showing cagrs between all vector elements?
#' @param time A vector referring to the time values of \code{x} for use in the matrix.

#' @return cagr(s) of vector \code{x}.
#' @export
#'
#' @examples
#' cagr(1:10)
#' cagr(1:10, matrix = TRUE)
#' cagr(1:10, matrix = TRUE, time = 1991:2000)

cagr <- function(x,
                 matrix = FALSE,
                 time = NULL){
  if(length(x) != length(time) & !is.null(time)) stop("Length of x and time must be equal.")
  len <- length(x)
  if(matrix == F){
    output <- ((x[len]/x[1]) ^ (1/(len-1)) - 1)
    return(output)

  } else {
    mat <- matrix(NA, len-1, len-1)
    for(i in 1:(len-1)){
      for(j in (1+i):len){
        mat[j-1,i] <- ((x[j]/x[i]) ^ (1/(j-i)) - 1)
      }
    }
    if(is.null(time)){
      rownames(mat) <- 2:len
      colnames(mat) <- 1:(len-1)
      return(mat)
    } else {
      rownames(mat) <- time[2:len]
      colnames(mat) <- time[1:len-1]
      return(mat)
    }
  }
}
