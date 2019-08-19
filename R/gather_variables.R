#' Gathers multiple variables.
#'
#'\code{gather_variables} gathers variables from a particularly formated dataframe. View examples for details.
#'
#' @param x A dataframe.
#' @param start Column index of where variables to be gathered starts
#' @param end Column index of where variables to be gathered ends
#' @param timeInd Column index of time variable
#' @param entity.position Before or After. Are the entity names before or after the seperator in the variable names?
#'
#' @return A long tidy dataframe of \code{x} with entities merged into one variable.
#' @export
#'
#' @examples
#' df <- data.frame(x = 2010:2018, CAN_Bonds = runif(9), US_Bonds = runif(9),
#' CAN_Debt = rnorm(9, 100, 30), US_Debt = rnorm(9, 100, 30))
#'
#' gather_variables(df)
gather_variables <- function(x,
                             start = 2,
                             end = length(x),
                             timeInd = 1,
                             entity.position = "before"){

  x <- x[,c(timeInd, c(1:length(x))[-timeInd])]
  names(x) <- c("YEAR", names(x)[-1])
  x$YEAR <- as.numeric(as.character(x$YEAR))
  df <- tidyr::gather(x, "Key", "Value", start:end)
  if(entity.position == "before"){
    df <- tidyr::separate(df, Key, c("Entity", "Variable"))
  } else df <- tidyr::separate(df, Key, c("Variable", "Entity"))

  df <- df %>%
    tidyr::spread(Variable, Value) %>%
    dplyr::arrange(Entity, YEAR)

  return(df)
}
