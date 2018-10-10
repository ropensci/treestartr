#' Utility function to check taxon_names in parsed data file
#'
#' @param tax_frame Dataframe of taxa parsed from datafile.
#' @return None
#'
taxon_testr <- function(tax_frame) {
    if (all(grepl("_", tax_frame$taxon))) {
    }
    else {
      warning("Taxon dataframe tips must be formmatted in genus_species format.
              If this is a higher order taxon with no species name, please                   format as taxon_sp.")
      stop
    }
    message("Taxon dataframe names formatted correctly")
    return(0)
  }
