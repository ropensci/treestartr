#' Print subtrees with tips added.
#' @description Once a tip has been added to the tree, print the newick-formatted subtree to which the tip was added.
#' @param tree Tree to which the tip will be added; object of type phylo
#' @param mrca_list Vector of taxa. The added tip will subtend the MRCA of these taxa
#' @param tip Taxon which will be added to the tree.
#' @return tree subtree to which tip was added
#' @importFrom ape extract.clade
#' @importFrom ape write.tree
#' @importFrom phytools findMRCA
#' @examples
#' mrca_list <- c("Ursus_arctos", "Ursus_spelaeus", "Ursus_americanus")
#' tree <- text_placr(tree, mrca_df)
#' echo_sub <- echo_subtree(tree, mrca_list, "Ursus_abstrusus")
#' @export
#'
echo_subtree <- function(tree, mrca_list, tip){
  mrca_list <- append(mrca_list, tip)
  loc <- phytools::findMRCA(tree, mrca_list)
  clade <- ape::extract.clade(tree, loc)
  echo_tree <- ape::write.tree(clade)
  return(echo_tree)
}
