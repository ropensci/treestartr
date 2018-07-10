test_that('Test dataf_parsR(dataf)',{
  dataf <- dataf_parsR('testdata/bears_taxa.tsv')
  expect_equal(nrow(dataf), 22)
  expect_equal(ncol(dataf), 2)
})
