#' Add tips to the tree based on taxonomy
#' @description Add tips not on existing tree to the tree via an MRCA node,
#' if they have congeners
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @return tree. Phylo object containing the starting tree,
#'          and all tips that were added.
#' @examples
#' genera_tree <- present_tippr(tree, absent_list)
#' @export
#'

present_tippr <- function(tree, absent_list){
#Check tree is of object phylo
  if (!inherits(tree, "phylo")){
    stop("tree must be of class 'phylo'")
  }
#Get all the taxa that are not on the tree, but have congeners on the tree
  found_df <- get_found(absent_list, tree)
  tree_df <- make_treedf(tree)

  for (row in 1:nrow(found_df)) {
    print("Adding tips with congeners on tree:")
    gen <- found_df[row, "A"]
    full <- as.character(found_df[[row, "B"]])
    print(full)
#Locate MRCA on tree for each set of congeners.
    mrca_list <- list()
    mrca_list <- tree_df$fullnames[tree_df$genera == gen]
    if (length(mrca_list) > 1) {
      loc <- findMRCA(tree, mrca_list)
      sprintf("Adding tip via MRCA at %d", loc)
#Place tip subtending MRCA of congeners.
      tree <- bind.tip(tree, full, where = loc)
    }else if (length(mrca_list) <= 1) {
#If one congener, new tip will subtend parent node of congener.
      print("Adding tip via parent node")
      loc <- getParent(tree, mrca_list)
      tree <- bind.tip(tree, full, where = loc)
    }
  }
  return(tree)
}
