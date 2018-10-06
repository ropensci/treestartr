#' Add tips to the tree based on taxonomy
#' @description Add tips not on existing tree to the tree via an MRCA node,
#' if they have congeners
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @param echo_subtrees Boolean; Print newick subtree with missing taxa added to screen
#' @param echo_revbayes Boolean; Print newick subtree with missing taxa added to screen, formatted for RevBayes fossilized birth-death analysis
#' Default FALSE.

#' @return tree. Phylo object containing the starting tree,
#'          and all tips that were added.
#' @examples
#' genera_tree <- present_tippr(tree, absent_list)
#' @export
#'

present_tippr <- function(tree, absent_list, echo_subtrees = NULL, echo_revbayes = NULL){
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
        echo_tree <- ape::extract.clade(tree, loc)
        message(ape::write.tree(echo_tree))
      }
      if (!is.null(echo_revbayes)){
        mrca_list <- append(mrca_list, full)
        quote_vec <-paste0('"', mrca_list, '"')
        q_vec <-paste0(quote_vec[-length(quote_vec)], ',')
        q_final <- append(q_vec, tail(quote_vec, n=1))
        #        message(sprintf("clade(%s", quote_vec, ')'))
        cat("clade(", q_final, ")")
      }
    }else if (length(mrca_list) <= 1) {
#If one congener, new tip will subtend parent node of congener.
      loc <- getParent(tree, mrca_list[1])
      message(sprintf("Adding tip %s", full, " via parent node  %s", loc))
      tree <- suppressWarnings(bind.tip(tree, full, where = loc))
      if (!is.null(echo_revbayes)){
        mrca_list <- append(mrca_list, full)
        quote_vec <-paste0('"', mrca_list, '"')
        q_vec <-paste0(quote_vec[-length(quote_vec)], ',')
        q_final <- append(q_vec, tail(quote_vec, n=1))
      }
      if (!is.null(echo_subtrees)){
        message(sprintf("Subtree: %", c(mrca_list, full)))
    }
    }
  }
  return(tree)
}
