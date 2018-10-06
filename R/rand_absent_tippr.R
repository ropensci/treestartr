#' Random addition of taxa
#' @description Add tips not on existing tree to the tree at random, if they do
#' not have congeners on the tree.
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the
#' tree
#' @param echo_subtrees Boolean; Print newick subtree with missing taxa added to screen
#' @param echo_revbayes Boolean; Print newick subtree with missing taxa added to screen, formatted for RevBayes fossilized birth-death analysis
#' Default FALSE.
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
#' @examples
#' new_tree <- rand_absent_tippr(tree, absent_list)
#' @export
#'

rand_absent_tippr <- function(tree, absent_list, echo_subtrees = NULL, echo_revbayes = NULL){
#Check that tree is of object phylo
  if (!inherits(tree, "phylo")){
    stop("tree must be of class 'phylo'")
  }
  if (is.null(echo_subtrees)){
  } else{
    message("Echoing Subtrees to Screen")
  }
  if (is.null(echo_revbayes)){
  } else{
    message("Echoing RevBayes-formatted Subtrees to Screen")
  }
  lost_df <- get_lost(absent_list, tree)

  for (row in seq_len(nrow(lost_df))) {
    full <- as.character(lost_df[[row, "full_name"]])
    message("Adding tips at random node: ", full)
    nodel <- tree$edge[, 2]
    num <- sample(nodel, 1)
    tree <- suppressWarnings(bind.tip(tree, full, where = num))
    if (!is.null(echo_revbayes)){
      parent <- getParent(tree, num)
      sub_list <- ape::extract.clade(tree, parent)
      quote_vec <-paste0('"', c(sub_list$tip.label, full), '"')
      q_vec <-paste0(quote_vec[-length(quote_vec)], ',')
      q_final <- append(q_vec, tail(quote_vec, n=1))
      cat("clade(", q_final, ")", '\n')
    }
    if (!is.null(echo_subtrees)){
      parent <- getParent(tree, num)
      sub_list <- ape::multi2di(ape::extract.clade(tree, parent))
      cat("Subtree:", ape::write.tree(sub_list), '\n')
    }
  }
  return(tree)
}
