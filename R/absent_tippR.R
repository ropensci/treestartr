#' Add tips not on existing tree to the tree, if thye do not have congeners on the tree
#'
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @return tree. Phylo object containing the starting tree,
#'          and all tips that were added.
#' @export
#'

absent_tippr <- function(tree, absent_list){
#Ensure our tree is a phylo object.
    if (!inherits(tree, "phylo")){
    stop("tree must be of class 'phylo'")
    }
#Get list of taxa with no congeners on tree
  lost_df <- get_lost(absent_list, tree)
#Iterate over lost_df, adding these tips to tree
  for (row in 1:nrow(lost_df)) {
    full <- as.character(lost_df[[row, "B"]])
    print("Adding tips:")
    print(full)
    plot(tree)
    nodelabels()
#Add tips to tree via user input
    num <- readline(cat(sprintf("Where would you like to put %s Enter
                                a node number from the tree that popped up",
                                full)) )
    num <- as.numeric(unlist(strsplit(num, ",")))
    tree <- bind.tip(tree, full, where = num)
  }
  return(tree)
}
