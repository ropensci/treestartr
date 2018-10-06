#' Add tips to tree via taxon list
#' @description Add tips according to csv or tsv file of taxon names and taxa
#' that form the clade
#' into which you"d like to insert the tip
#' @param tree Starting tree; object of type phylo
#' @param mrca_df Dataframe containing a column of the taxa you'd like to place,
#'                and one column with the clade into which you'd like to place
#'                it
#' @param echo_subtrees Boolean; Print newick subtree with missing taxa added #' to screen
#' @param echo_revbayes Boolean; Print newick subtree with missing taxa added #' to screen, formatted for RevBayes fossilized birth-death analysis
#' Default FALSE.
#' @return tree Phylo object containing the starting tree,
#'          and all tips that were added.
#' @examples
#' text_placr(tree, mrca_df)
#' @export

text_placr <- function(tree, mrca_df, echo_subtrees = NULL, echo_revbayes = NULL){
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
    tree <- suppressWarnings(bind.tip(tree, tax, where = loc))
    if (!is.null(echo_revbayes)){
      quote_vec <-paste0('"', c(mrca_list, tax), '"')
      q_vec <-paste0(quote_vec[-length(quote_vec)], ',')
      q_final <- append(q_vec, tail(quote_vec, n=1))
      cat("clade(", q_final, ")", '\n')
    }
    if (!is.null(echo_subtrees)){
      parent <- getParent(tree, loc)
      sub_list <- ape::multi2di(ape::extract.clade(tree, parent))
      cat("Subtree:", ape::write.tree(sub_list), '\n')
    }
  }
  return(tree)
  }
