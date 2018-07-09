#' Load morpohological matrices to get taxon lists
#'
#' @param morphDat A Nexus file containing morphological matrix
#' @return A vector of names present in either the matrix
#' @import ape
#' @export

parse_morphology <- function(morphDat){
  mm <- read.nexus.data(morphDat)
  morph_ns <- names(mm)
  return(morph_ns)
}

