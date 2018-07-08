library(testthat)
library(ape)

test_that('Test rand_absent_tippR(absent_list, tree)',{
  tree <- read.nexus('testdata/simple.tre')
  tax_list <- dataf_parsR('testdata/bears_taxa.tsv')
  absent_list <- genera_strippR(tree, tax_list)
  tree1 <- rand_absent_tippR(absent_list, tree)
  tree1 <- multi2di(tree1, random=TRUE)
  expect_equal(tree1$Nnode, 23)
})
