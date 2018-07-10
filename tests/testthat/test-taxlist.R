library(phytools)

test_that('Test genera_strippR(tree, tax_list)',{
  tree <- read.nexus('testdata/simple.tre')
  tax_list <- dataf_parsR('testdata/bears_taxa.tsv')
  expect_equal(length(genera_strippR(tree, tax_list)), 7)
})
