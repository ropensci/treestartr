#' Add tips not on existing tree to the tree, if thye do not have congeners on the tree
#'
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @param tree Starting tree; object of type phylo
#' @return tree. Phylo object containing the starting tree,
#'          and all tips that were added.
#' @import ape
#' @import phytools
#' @export
#'

absent_tippR <- function(absent_list, tree){
  if(!inherits(tree,"phylo")){
    stop("tree must be of class 'phylo'")
  }
  lost_df <- get_lost(absent_list, tree)

  for (row in 1:nrow(lost_df)) {
    full <- as.character(lost_df[[row, "B"]])
    print('Adding tips:')
    print(full)
    plot(tree)
    nodelabels()
    num <- readline(cat(sprintf("Where would you like to put %s Enter a node number
                                from the tree that popped up", full)) )
    num <- as.numeric(unlist(strsplit(num, ",")))
    pos <- 0.4*(tree$edge.length[which(tree$edge[,2]==num)])
    tree <- bind.tip(tree,full,where=num)
  }
  return(tree)
}
