#' Add tips to the tree based on taxonomy.
#' @description Add tips not on existing tree to the tree via an MRCA node,
#' if they have congeners. This function will look to see if any tips on the
#'  tree have the same genus as the tip to be added. If there are multiple
#'   members of the genus, the tip will be added subtending the MRCA of all
#'    present congeners. If there is member of the genus, the tip will be added
#'     subtending the parent node of the congener.
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @param echo_subtrees Boolean; Print newick subtree with missing taxa added to screen. Default FALSE.
#' @param echo_revbayes Boolean; Print clade constraints with missing taxa added to screen, formatted for RevBayes fossilized birth-death analysis. Default FALSE.

#' @return tree. Phylo object containing the starting tree,
#'          and all tips that were added.
#' @examples
#' genera_tree <- present_tippr(tree, absent_list)
#' @export
#'

present_tippr <- function(tree, absent_list, echo_subtrees = NULL,
                          echo_revbayes = NULL){
#Check tree is of object phylo
  if (!inherits(tree, "phylo")){
    stop("tree must be of class 'phylo'")
  }
#Get all the taxa that are not on the tree, but have congeners on the tree
  found_df <- get_found(absent_list, tree)
  tree_df <- make_treedf(tree)
  message("Tree tip names formatted correctly")
  if (is.null(echo_subtrees)){
  } else{
    message("Echoing Subtrees to Screen")
  }
  if (is.null(echo_revbayes)){
  } else{
    message("Echoing RevBayes-formatted Subtrees to Screen")
  }

  for (row in seq_len(nrow(found_df))) {
    gen <- found_df[row, "genera"]
    full <- as.character(found_df[[row, "full_name"]])
#Locate MRCA on tree for each set of congeners.
    mrca_list <- list()
    mrca_list <- tree_df$fullnames[tree_df$genera == gen]
    if (length(mrca_list) > 1) {
      loc <- findMRCA(tree, mrca_list)
      message(sprintf("Adding tip %s", full, " to MRCA at node %s", loc))
#Place tip subtending MRCA of congeners.
      tree <- suppressWarnings(bind.tip(tree, full, where = loc))
      if (!is.null(echo_subtrees)){
        e_t <- echo_subtree(tree, mrca_list, tip)
        cat("Subtree: ", e_t)
      }
      if (!is.null(echo_revbayes)){
        q_final <- echo_rb(tree, mrca_list, full)
        cat("clade(", q_final, ")")
      }
    }else if (length(mrca_list) <= 1) {
#If one congener, new tip will subtend parent node of congener.
      num <- which(tree$tip.label %in% mrca_list[1])
      loc <- getParent(tree, num)
      message(sprintf("Adding tip %s", full, " via parent node  %s", loc))
      tree <- suppressWarnings(bind.tip(tree, full, where = loc))
      if (!is.null(echo_revbayes)){
        q_final <- echo_rb(tree, mrca_list, full)
        cat("clade(", q_final, ")")
      }
      if (!is.null(echo_subtrees)){
        e_t <- echo_subtree(tree, mrca_list, tip)
        cat("Subtree: ", e_t)
    }
    }
  }
  return(tree)
}
