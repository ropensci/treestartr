library(phytools)

test_that("Test present_tippr(absent_list, tree)", {
  tree <- read.nexus("testdata/simple.tre")
  tax_list <- dataf_parsr("testdata/bears_taxa.tsv")
  absent_list <- genera_strippr(tax_list, tree)
  tree1 <- present_tippr(absent_list, tree)
  tree1 <- multi2di(tree1, random = TRUE)
  expect_equal(tree1$Nnode, 21)
})
