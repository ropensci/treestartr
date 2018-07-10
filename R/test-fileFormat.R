fileformat <- function(dataf) {
if (endsWith(dataf, ".tsv") == FALSE & endsWith(dataf, ".csv") == FALSE){
  stop("This is not a supported file format. Please load in the
        total set of taxa as a TSV or CSV file.")
} else {
  print("File format is appropriate")
}
  return(0)
}
