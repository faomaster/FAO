#' Weighted quantiles for multiple variables.
#'
#'\code{quantvar} produces weighted quantiles corresponding to a specified variable and frequency. View examples for details.
#'
#'First column of dataframe must be weight variable, second must be sorting variable. Function provides best
#'approximation on dataframes with greater than 10,000 observations.
#'
#' @param df A dataframe.
#' @param freq A numeric value that specifies quantile frequency. Default is 10, i.e. decile.
#'
#' @return A dataframe with summary statistics for each quantile frequency.
#' @export
#'
#' @examples
#' df <- data.frame(weights = rnorm(1000,1000,50), income = rnorm(1000,35000,5000))
#' quantvar(df,freq=5)

quantvar <- function(df, freq=10) {

sum1 <- sum(df[,1])

rnk1 <- order(df[,2],decreasing = FALSE)

foo3 <- df[rnk1,]

foo4 <- as.data.frame(cbind(cumsum(foo3[,1]),foo3))

cut1 <- freq

fin6 = NULL

for (loop in 1:cut1) {

  cut2 <- ((loop-1) * (100/cut1)) / 100

  cut3 <- (loop * (100/cut1)) / 100

  foo5 <- foo4[foo4[,1] > (cut2*sum1) & foo4[,1] <= (cut3*sum1) , ]

  size <- as.numeric(ncol(foo5))

  fin4 = NULL

  for (boop in 1:(size-2)) {

    fin1 <- as.data.frame(crossprod(foo5[,(boop+2)],foo5[,(2)])/1000000)
    fin2 <- as.data.frame((crossprod(foo5[,(boop+2)],foo5[,(2)])/sum(foo5[,2]))/1000)
    fin3 <- cbind(fin1,fin2)

    fin4 <- dplyr::bind_cols(fin4,fin3)

    }

    fin5 <- cbind(cut2,cut3,sum(foo5[,2])/1000000,fin4)

    colnames(fin5) <- rep("X",((size-2)*2)+3)

    fin6 <- rbind(fin6,fin5)

}

return(fin6)
}
