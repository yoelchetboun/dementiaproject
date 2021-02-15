#' Load Rdata in a R variable
#'
#' @param fileName char Rdata filename
#'
#' @return variable
#' @export

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
