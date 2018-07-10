#' Add tips according to csv or tsv file of taxon names and taxa that form the clade
#' into which you'd like to insert the tip
#'
#' @param mrca_df Dataframe containing a column of the taxa you'd like to place,
#'                and one column with the clade into which you'd like to place it
#' @param tree Starting tree; object of type phylo
#' @return tree. Phylo object containing the starting tree,
#'          and all tips that were added.
#' @export

text_placR <- function(tree, mrca_df){
  iter <- unique(as.character(mrca_df$taxon))
  for (tax in iter) {
    x <- sprintf('Placing taxon via provided input: %s', tax)
    print(x)
    mrca_list <- mrca_df$clade[mrca_df$taxon == tax]
    mrca_list <- as.vector(mrca_list)
    loc <- findMRCA(tree, mrca_list)
    x <-  sprintf('Placing tip at node %d', loc)
    print(x)
    tree <- bind.tip(tree, tax, where=loc)
  }
  return(tree)
  }
