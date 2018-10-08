#' Determine which tips are not on the tree, and get their genera.
#'
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @return absent_df Dataframe objects expressing which tips are in the total set
#'         but not the tree, and their genera
#' @examples
#' not_present <- treestartr:::make_absentdf(absent_list)

make_absentdf <- function(absent_list){
  absent_df <- data.frame(matrix(ncol = 2, nrow = length(absent_list)))
  x <- c("genera", "fullnames")
  colnames(absent_df) <- x
  absent_df$genera <- lapply(strsplit(absent_list, "_"), `[`, 1)
  absent_df$fullnames <- absent_list
  return(absent_df)
}
