#' Reads all csv files from a directory.
#'
#' @param path A string to a windows directory.
#'
#' @return A list of objects read from \code{path}.
#' @export
#'
#' @examples
#' read_dir_csv("P:/Knowledge/Data Resources/Microdata/GSS")

read_dir_csv <- function(path = getwd()){
  fileNames <- list.files(path)
  fileNames <- fileNames[grepl("(.)+\\.csv", fileNames)]
  tableList <- lapply(fileNames, function(f){
    read.csv(paste0(path, "/", f))})
  names(tableList) <- fileNames
  return(tableList)
}
