#' Load molecular matrices to get taxon lists of each
#'
#' @param molDat A Nexus file containing molecular matrix
#' @return mol_ns A vector of names present in either the matrix
#' @export

parse_moleculaR <- function(molDat){
  mol_ns <- read.nexus.data(molDat)
  mol_ns <- names(mol_ns)
  return(mol_ns)
}
