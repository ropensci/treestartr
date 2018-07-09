#' Determine which tips are on the tree, and get their genera.
#'
#' @param tree Starting tree; object of type phylo
#' @return dataframe Dataframe objects expressing the tips, and their genera
#' @import phytools
#'

make_treedf <- function(tree){
  if(!inherits(tree,"phylo")){
    stop("tree must be of class 'phylo'")
  }
  tree_df <- data.frame(matrix(ncol = 2, nrow = length(tree$tip.label)))
  x <- c("genera", "fullnames")
  colnames(tree_df) <- x
  tree_df$genera <- sapply(strsplit(tree$tip.label, "_"), `[`, 1)
  tree_df$fullnames <- tree$tip.label
  return(tree_df)
}


