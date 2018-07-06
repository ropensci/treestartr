#' See which morphological & stratigraphic genera are represented
#' in the molecular tree
#'
#' @param tree Starting phylogeny
#' @param total_set Total set of taxa on tree
#'
genera_strippR <- function(tree, tax_list){
  total_set <- unname(unlist(lapply(tax_list["taxon"], as.character)))
  (absent <- unlist(total_set[which(!total_set %in% tree$tip.label)]))
  return(absent)
}

