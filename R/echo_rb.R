#' Print RevBayes-formatted clade constriants.
#' @description Once a tip has been added to the tree, print the subtree to which the tip was added. The output of this function is printed for use as a RevBayes clade constraint object.
#' @param tree Tree to which the tip will be added; object of type phylo
#' @param mrca_list Vector of taxa. The added tip will subtend the MRCA of these taxa.
#' @param tip Taxon which will be added to the tree.
#' @return vector A comma-separated list of all the taxa in the subtree to which the tip was added.
#' @examples
#'  mrca_list <- c("Ursus_arctos", "Ursus_spelaeus", "Ursus_americanus")
#' clade_constraint <- echo_rb(tree, mrca_list, "Ursus_abstrusus")
#' @export
#'
echo_rb <- function(tree, mrca_list, tip){
  mrca_list <- append(mrca_list, tip)
  quote_vec <-paste0('"', mrca_list, '"')
  q_vec <-paste0(quote_vec[-length(quote_vec)], ',')
  q_final <- append(q_vec, tail(quote_vec, n=1))
  return(q_final)
}
