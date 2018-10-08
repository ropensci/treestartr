#' Utility function to check taxon_names in parsed data file
#'
#' @param dataf Taxon list
#' @return None
#'
taxon_testr <- function(tax_list) {
    if (all(grepl("_", tax_list$taxon))) {
    }
    else {
      warning("Taxon list tips must be formmatted in genus_species format. If
              this is a higher order taxon with no species name, please format               as taxon_sp.")
      stop
    }
    message("Taxon list names formatted correctly")
    return(0)
  }
