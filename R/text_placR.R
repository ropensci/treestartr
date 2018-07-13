#' Add tips to tree via taxon list
#' @description Add tips according to csv or tsv file of taxon names and taxa
#' that form the clade
#' into which you"d like to insert the tip
#' @param tree Starting tree; object of type phylo
#' @param mrca_df Dataframe containing a column of the taxa you'd like to place,
#'                and one column with the clade into which you'd like to place
#'                it
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
#' @examples
#' text_placr(tree, mrca_df)
#' @export

text_placr <- function(tree, mrca_df){
  iter <- unique(as.character(mrca_df$taxon))
#Get taxa to place
  for (tax in iter) {
#Find MRCA of povided taxa
    message("Placing tip ", tax)
    mrca_list <- mrca_df$clade[mrca_df$taxon == tax]
    mrca_list <- as.vector(mrca_list)
    message("via relatives ", mrca_list)
    loc <- findMRCA(tree, mrca_list)
    message(" at node ", loc)
#Place new tip subtending MRCA of provided taxa
    tree <- bind.tip(tree, tax, where = loc)
  }
  return(tree)
  }
