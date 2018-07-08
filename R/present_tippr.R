#' Add tips not on existing tree to the tree, if they have congeners
#'
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @param tree Starting tree; object of type phylo
#' @return tree. Phylo object containing the starting tree,
#'          and all tips that were added.
#' @import ape
#' @import phytools
#' @export
#'

present_tippR <- function(absent_list, tree){
  if(!inherits(tree,"phylo")){
    stop("tree must be of class 'phylo'")
  }

  found_df <- get_found(absent_list, tree)
  tree_df <- make_treedf(tree)

  for (row in 1:nrow(found_df)) {
    print('Adding tips with congeners on tree:')
    gen <- found_df[row, "A"]
    full <- as.character(found_df[[row, "B"]])
    print(full)
    mrca_list <- list()
    mrca_list <- tree_df$fullnames[tree_df$genera==gen]
    if (length(mrca_list) > 1) {
      loc <- findMRCA(tree, mrca_list)
      sprintf('Adding tip via MRCA at %d', loc)
      tree <- bind.tip(tree,full,where=loc)
    }else if (length(mrca_list) <= 1) {
      print('Adding tip via parent node')
      loc <- getParent(tree, mrca_list)
      tree <- bind.tip(tree, full, where=loc)
    }
  }
  return(tree)
}

