#' Determine which tips are not on the tree, and get their genera.
#'
#' @param absent_list Vector of taxa in the total dataset that are not on the tree
#' @return dataframe Dataframe objects expressing which tips are in the total set
#'         but not the tree, and their genera
#'

make_absentdf <- function(absent_list){
  absent_df <- data.frame(matrix(ncol = 2, nrow = length(absent_list)))
  x <- c("genera", "fullnames")
  colnames(absent_df) <- x
  absent_df$genera <- sapply(strsplit(absent_list, "_"), `[`, 1)
  absent_df$fullnames <- absent_list
  return(absent_df)
}