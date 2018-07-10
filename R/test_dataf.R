test_dataf <- function(tax_list) {
  assert_that(ncol(tax_list) == 2)
  assert_that(nrow(tax_list) > 0)
}
