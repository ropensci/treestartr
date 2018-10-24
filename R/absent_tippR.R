#'Add tips to tree via user input
#' @description Add tips not on existing tree to the tree via user input,
#' if they do not have congeners on the tree. This function will generate a
#'  tree, with nodes numbered. For each tip to be added, the user will be asked #'  to enter the number of the node they would like the tip to subtend.
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
#' @importFrom graphics plot
#' @examples
#' \dontrun{ new <- absent_tippr(tree, absent_list) }
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
  for (row in seq_len(nrow(lost_df))) {
    tip <- as.character(lost_df[[row, "full_name"]])
    suppressWarnings(plot(tree))
    ape::nodelabels()
    ape::tiplabels()
#Add tips to tree via user input
    cat("Refer to the tree that popped up to place taxon ", tip, '\n')
    num <- readline("At which node would you like to place the tip? Enter a number.")
    num <- as.numeric(unlist(strsplit(num, ",")))
    tree <- suppressWarnings(phytools::bind.tip(tree, tip, where = num))
  }
  return(tree)
}
