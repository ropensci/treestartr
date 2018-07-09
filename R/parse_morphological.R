#' Load morpohological matrices to get taxon lists
#'
#' @param morphDat A Nexus file containing morphological matrix
#' @return morph_ns A vector of names present in either the matrix
#' @export

parse_morphology <- function(morphDat){
  morph_ns <- ape::read.nexus.data(morphDat)
  morph_ns <- names(morph_ns)
  return(morph_ns)
}

