#' Detect file format of taxon file
#' @description Detect file format of the total taxon list (molecular, morphological, and stratigraphic) and maximum age of fossil, see RevBayes total
#' -evidence fossil file for an example of this.
#' @param dataf A data frame with one column containing the taxon name of tips in phylogenetic tree. Optionally, a second column can indicate the maximum age of the tip. If the tip is extant, use 0.0 as the age. Can be CSV or TSV.
#' @return tax_frame Dataframe containing the total set of tips on the tree
#' @importFrom utils read.csv
#' @examples
#' \dontrun{tax_frame <- dataf_parsr(dataf)}
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
    names(df) <- c("taxon", "age")
    tax_frame <- df
    message("File processing complete.")
  } else if (ncol(df) == 1) {
    names(df) <- "taxon"
    tax_frame <- df
    message("File processing complete.")
  } else {
    stop("Dataframes should either one or two columns, minimally a taxon
         column")
  }
  taxon_testr(tax_frame)
  tax_frame <- as.data.frame(tax_frame)
  return(tax_frame)
}
