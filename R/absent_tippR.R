#'Add tips to tree via user input
#' @description Add tips not on existing tree to the tree via user input,
#' if they do not have congeners on the tree. This function will generate a
#'  tree, with nodes numbered. For each tip to be added, the user will be asked #'  to enter the number of the node they would like the tip to subtend.
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @param echo_subtrees Boolean; Print newick subtree with missing taxa added to screen. Default FALSE.
#' @param echo_revbayes Boolean; Print clade constraints with missing taxa added to screen, formatted for RevBayes fossilized birth-death analysis. Default FALSE.
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
#' @examples
#' \dontrun{ new <- absent_tippr(tree, absent_list) }
#' @export
#'

absent_tippr <- function(tree, absent_list, echo_subtrees = NULL, echo_revbayes = NULL){
#Ensure our tree is a phylo object.
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
#Get list of taxa with no congeners on tree
  lost_df <- get_lost(absent_list, tree)
#Iterate over lost_df, adding these tips to tree
  for (row in seq_len(nrow(lost_df))) {
    full <- as.character(lost_df[[row, "full_name"]])
    plot(tree)
    ape::nodelabels()
#Add tips to tree via user input
    cat("Refer to the tree that popped up to place taxon ", full, '\n')
    num <- readline("At which node would you like to place the tip? Enter a number.")
    num <- as.numeric(unlist(strsplit(num, ",")))
    tree <- suppressWarnings(phytools::bind.tip(tree, full, where = num))
    if (!is.null(echo_revbayes)){
      e_t <- echo_subtree(tree, mrca_list, tip)
      cat("Subtree: ", e_t, "\n")
    }
    if (!is.null(echo_subtrees)){
      q_final <- echo_rb(tree, mrca_list, full)
      cat("clade(", q_final, ")\n")
    }
  }
  return(tree)
}
