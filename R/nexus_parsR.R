#' Load morpohological and molecular matrices to get taxon lists of each
#'
#' @param morphDat A Nexus file containing morphological matrix
#' @param molDat A Nexus file containing molecular matrix
#' @return A vector of names present in either the matrix
#' @import ape
#' @export

parse_morphology <- function(morphDat){
  mm <- read.nexus.data(morphDat)
  morph_ns <- names(mm)
  return(morph_ns)
}

parse_moleculaR <- function(molDat){
  mol <- read.nexus.data(morphDat)
  mol_ns <- names(mol)
  return(mol_ns)
}
