#'Add tips to tree via user input
#' @description Add tips not on existing tree to the tree via user input,
#' if they do not have congeners on the tree
#' @param tree Starting tree; object of type phylo
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
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
  for (row in 1:nrow(lost_df)) {
    full <- as.character(lost_df[[row, "B"]])
    message("Adding tips: ", full)
    plot(tree)
    ape::nodelabels()
#Add tips to tree via user input
    message("Refer to the tree that popped up. Where would you like to put %s ",                  full)
#    num <- readline(cat(sprintf("Where would you like to put %s ",
#                                full)) ) }
      num <- readLines(file("stdin"), n = 1L)
         num <- as.numeric(unlist(strsplit(num, ",")))
    tree <- phytools::bind.tip(tree, full, where = num)
  }
  return(tree)
}
