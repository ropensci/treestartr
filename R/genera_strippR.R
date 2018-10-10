#' Check if tip is on tree
#' @description Test which taxa in the total set of taxa are present on the
#' tree, and which need to be added.
#' @param tree Starting phylogeny, of type phylo
#' @param tax_frame Total set of taxa on tree, as dataframe.
#' @return absent_list of taxa that are present in the total set of trees, but not the starting tree
#' @examples
#' absent_taxa <- genera_strippr(tree, tax_frame = tax_frame)
#' @export
#'
genera_strippr <- function(tree, tax_frame){
#Check if tree is phylo object
    if (!inherits(tree, "phylo")){
    stop("tree must be of class 'phylo'")
    }
    if (! is.data.frame(tax_frame)){
    tax_frame <- as.data.frame(tax_frame)
    if (ncol(tax_frame) == 2){
      names(tax_frame) <- c("taxon", "age")
    } else if (ncol(tax_frame) == 1) {
      names(tax_frame) <- c("taxon")
    }
    else {
      stop("Taxon frame must be a dataframe")
    }
  }
#Get taxa which are not on tree
  total_set <- unname(unlist(lapply(tax_frame["taxon"], as.character)))
  (absent <- unlist(total_set[which(!total_set %in% tree$tip.label)]))
  return(absent)
}
