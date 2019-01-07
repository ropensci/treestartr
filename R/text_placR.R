#' Add tips to tree via taxon list
#' @description Add tips according to csv or tsv file of taxon names and taxa
#' that form the clade into which you"d like to insert the tip. One column
#' should be called 'taxon', and should contain the taxon to be placed. The
#'  other column should be called 'clade' and contain the taxon with which the
#'   taxon to be placed will form a group. Each member of the clade will be
#' placed on its own line. An example of this file can be seen in `inst/extdat
#' a/mrca_df.tsv`
#' @param tree Starting tree; object of type phylo
#' @param mrca_df Dataframe containing a column of the taxa you'd like to place and one column with the clade into which you'd like to place it
#' @param echo_subtrees Boolean; Print newick subtree with missing taxa added to screen. Default FALSE.
#' @param echo_revbayes Boolean; Print clade constraints with missing taxa added to screen, formatted for RevBayes fossilized birth-death analysis. Default FALSE.
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
#' @importFrom ape getMRCA
#' @examples
#' text_placr(tree, mrca_df)
#' @export

text_placr <- function(tree, mrca_df, echo_subtrees = NULL,
                       echo_revbayes = NULL){
  iter <- unique(as.character(mrca_df$taxon))
#Get taxa to place
    for (tax in iter) {
    if (tax %in% tree$tip.label){
      next
    }
    message("Placing tip ", tax)
    mrca_list <- mrca_df$clade[mrca_df$taxon == tax]
    if (length(mrca_list) == 1) {
      mrca_list <- as.vector(mrca_list)
      message("as sister to ", mrca_list)
      loc <- which(tree$tip.label %in% mrca_list)
      message(" at node ", loc)
    } else{
    mrca_list <- as.vector(mrca_list)
    q_vec <-paste(as.character(mrca_list), collapse=", ")
    message("via relatives ", q_vec)
    loc <- ape::getMRCA(tree, mrca_list)
    message(" at node ", loc)
    }
#Place new tip subtending MRCA of provided taxa
    tree <- suppressWarnings(bind.tip(tree, tax, where = loc))
    if (!is.null(echo_revbayes)){
      q_final <- echo_rb(tree, mrca_list, tax)
      cat("clade(", q_final, ")")
    }
    if (!is.null(echo_subtrees)){
      e_t <- echo_subtree(tree, mrca_list, tax)
      cat("Subtree: ", e_t)
    }
  }
  return(tree)
  }
