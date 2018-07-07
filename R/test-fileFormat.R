#' @param dataf A data frame with one column containing the taxon name of tips in
#' phylogenetic tree, and one column indicating the maximum age of the tip. If
#' the tip is extant, use 0.0 as the age. Can be CSV or TSV.
#' @return 0
#' @export

fileFormat <- function(dataf) {
if (endsWith(dataf, '.tsv') == FALSE & endsWith(dataf, '.csv') == FALSE){
  stop('This is not a supported file format. Please load in the total set of taxa
          as a TSV or CSV file.')
} else {
  print("File format is appropriate")
}
  return(0)
}

