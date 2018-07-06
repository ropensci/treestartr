#' Detect file format of taxon list (molecular, morphological, and stratigraphic) and maximum age of fossil, see RevBayes
#' total-evidence fossil file for an example of this.
#'
#' @param dataf A data frame with one column containing the taxon name of tips in
#' phylogenetic tree, and one column indicating the maximum age of the tip. If
#' the tip is extant, use 0.0 as the age.

dataf_parsR <- function(dataf) {
  if (endsWith(dataf, '.tsv') == TRUE) {
    df = read.csv(file = dataf, sep = "\t")
  } else {
    df = read.csv(file = dataf)
  }
  tax_list= df[c('taxon', 'age')]
  return(tax_list)
}
