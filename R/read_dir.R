#' Read an entire directory
#'
#'\code{read_dir} Reads all files with specified extension(s) from a directory.
#'
#'Files types currently supported include .csv, .xlsx and .txt. All .txt files will be read as csv files.
#'
#' @param path A string to a windows directory.
#' @param type A vector of strings containing file extensions.
#'
#' @return A list of objects read from \code{path}.
#' @export
#'
#' @examples
#' read_dir("P:/Knowledge/Data Resources/Microdata/GSS")

read_dir <- function(path = getwd(), type = c("csv")){
  map <- matrix(c("csv", "txt", "xlsx",
                  "csv", "csv", "xlsx"), ncol = 2)
  if(!(all(type %in% map[,1]))) stop("A file type specified is not supported")
  fileNames <- list.files(path)
  read_type <- function(li, path, type){
    if(length(type) == 0) return(li)
    typeFiles <- fileNames[grepl(paste0("(.)+\\.", type[1]), fileNames)]
    tblList <- lapply(typeFiles, function(f){
      match.fun(paste0("read.", map[map[,1] == type[1]][2]))(paste0(path, "/", f))})
    names(tblList) <- typeFiles
    tblList <- c(li, tblList)
    read_type(tblList, path, type[-1])
  }
  return(read_type(list(), path, type))
}
