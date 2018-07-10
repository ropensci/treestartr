#' See which morphological & stratigraphic genera are represented
#' in the molecular tree
#'
#' @param tree Starting phylogeny, of type phylo
#' @param tax_list Total set of taxa on tree, as dataframe
#' @return absent list of taxa that are present in the total set of trees, but not the
#'        starting tree
#' @export
#'
genera_strippr <- function(tree, tax_list){
#Check if tree is phylo object
    if (!inherits(tree, "phylo")){
    stop("tree must be of class 'phylo'")
    }
#Get taxa which are not on tree
  total_set <- unname(unlist(lapply(tax_list["taxon"], as.character)))
  (absent <- unlist(total_set[which(!total_set %in% tree$tip.label)]))
  print(absent)
  return(absent)
}
