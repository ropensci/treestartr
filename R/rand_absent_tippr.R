#' Add tips not on existing tree to the tree at random, if they do not have congeners on the tree.
#'
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @import phytools
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
#' @export
#'

rand_absent_tippr <- function(tree, absent_list){
#Check that tree is of object phylo
  if (!inherits(tree, "phylo")){
    stop("tree must be of class 'phylo'")
  }
  lost_df <- get_lost(absent_list, tree)

  for (row in 1:nrow(lost_df)) {
    full <- as.character(lost_df[[row, "B"]])
    sprintf("Adding tips at random: %s", full)
    nodel <- tree$edge[, 2]
    num <- sample(nodel, 1)
    tree <- bind.tip(tree, full, where = num)
  }
  return(tree)
}
