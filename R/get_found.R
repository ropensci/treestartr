#' Get dataframe of tips that do not have congeners on tree.
#' @description Determine which tips that are not on the tree, and checks if they have congeners
#' on the tree.
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @param tree Starting tree; object of type phylo
#' @return found_df Dataframe objects expressing the tips that are not
#'         on the tree, if they have congeners on the tree
#' @examples
#' has_congeners <- get_found(absent_list, tree)
#' @export

get_found <- function(absent_list, tree){
  absent_df <- make_absentdf(absent_list)
  tree_df <- make_treedf(tree)

  tree_gen <- data.frame(matrix(ncol = 2, nrow = length(absent_df)))
  not_gen <- data.frame(matrix(ncol = 2, nrow = length(absent_df)))
  x <- c("genera", "fullnames")
  colnames(tree_gen) <- x
  colnames(not_gen) <- x

  found_gen <- list()
  found_full <- list()

  for (row in seq_len(nrow(absent_df))) {
    gen <- absent_df[row, "genera"]
    full <- absent_df[row, "fullnames"]
    found_gen[[row]] <- gen[which(gen %in% tree_df$genera)]
    found_full[[row]] <- full[which(gen %in% tree_df$genera)]
  }
  found_gen <- found_gen[lapply(found_gen, length) > 0]
  found_full <- found_full[lapply(found_full, length) > 0]
  found_df <- do.call(rbind.data.frame, Map('c', found_full, found_gen))
  names(found_df ) <- c("full_name", "genera")
  return(found_df)
}
