#' Detect file format of taxon file
#' @description Detect file format of taxon list (molecular, morphological, and
#' stratigraphic) and maximum age of fossil, see RevBayes total-evidence fossil
#' file for an example of this.
#' @param dataf A data frame with one column containing the taxon name of tips in
#' phylogenetic tree, and one column indicating the maximum age of the tip. If
#' the tip is extant, use 0.0 as the age. Can be CSV or TSV.
#' @return tax_list Dataframe containing the total set of tips on the tree
#' @examples
#' \dontrun{taxa_df <- dataf_parsr(dataf)}
#' @export

dataf_parsr <- function(dataf) {
  fileformat(dataf)
  if (endsWith(dataf, ".tsv") == TRUE) {
    message(".tsv file ending detected.")
    df <- read.csv(file = dataf, sep = "\t")
  } else if (endsWith(dataf, ".csv") == TRUE){
    message(".csv file ending detected.")
    df <- read.csv(file = dataf)
  }
  if (ncol(df) == 2){
  tax_list <- df[c("taxon", "age")]
  message("File processing complete.")
  } else if (ncol(df) != 2) {
    stop("Dataframes should have two columns, taxon and ages")
  }
  return(tax_list)
}
