#' Test if dataframe has two columns and a non-zero number of rows.
#'
#' @param tax_list Dataframe containing a list of taxa in the analysis and their ages.
#' @return None
#' @export

test_dataf <- function(tax_list) {
  assert_that(ncol(tax_list) == 2)
  assert_that(nrow(tax_list) > 0)
}
