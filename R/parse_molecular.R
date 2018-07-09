#' Load molecular matrices to get taxon lists of each
#'
#' @param molDat A Nexus file containing molecular matrix
#' @return A vector of names present in either the matrix
#' @import ape
#' @export

parse_moleculaR <- function(molDat){
  mol <- read.nexus.data(morphDat)
  mol_ns <- names(mol)
  return(mol_ns)
}
