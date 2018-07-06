#' Add tips not on existing tree to the tree
#'
#' @param absent Vector of taxa in the total dataset that are not on the tree
#' @param tree Phylogeny
#'
library(ape)
library(phytools)
library(phylobase)

tippR <- function(absent, tree){
  tree_df <- data.frame(matrix(ncol = 2, nrow = length(tree$tip.label)))
  absent_df <- data.frame(matrix(ncol = 2, nrow = length(absent)))
  x <- c("genera", "fullnames")
  colnames(tree_df) <- x
  colnames(absent_df) <- x
  tree_df$genera <- sapply(strsplit(tree$tip.label, "_"), `[`, 1)
  tree_df$fullnames <- tree$tip.label
  absent_df$genera <- sapply(strsplit(absent, "_"), `[`, 1)
  absent_df$fullnames <- absent

  tree_gen <- data.frame(matrix(ncol = 2, nrow = length(absent_df)))
  not_gen <- data.frame(matrix(ncol = 2, nrow = length(absent_df)))
  x <- c("genera", "fullnames")
  colnames(tree_gen) <- x
  colnames(not_gen) <- x

  found_gen <- list()
  found_full <- list()

  not_found_gen <- list()
  not_found_full <- list()

  for (row in 1:nrow(absent_df)) {
    gen <- absent_df[row, "genera"]
    print(gen)
    full <- absent_df[row, "fullnames"]
    found_gen[[row]] <- gen[which(gen %in% tree_df$genera)]
    found_full[[row]] <- full[which(gen %in% tree_df$genera)]
    not_found_gen[[row]] <- gen[which(!gen %in% tree_df$genera)]
    not_found_full[[row]] <- full[which(!gen %in% tree_df$genera)]
  }
  found_gen <- found_gen[lapply(found_gen,length)>0]
  not_found_gen <- not_found_gen[lapply(not_found_gen,length)>0]
  found_full <- found_full[lapply(found_full,length)>0]
  not_found_full <- not_found_full[lapply(not_found_full,length)>0]
  found_df <- do.call(rbind, Map(data.frame, A=found_gen, B=found_full))
  not_found_df <- do.call(rbind, Map(data.frame, A=not_found_gen, B=not_found_full))

  for (row in 1:nrow(found_df)) {
  gen <- found_df[row, "A"]
  full <- as.character(found_df[[row, "B"]])
  print(full)
  mrca_list <- list()
  mrca_list <- tree_df$fullnames[tree_df$genera==gen]
  if (length(mrca_list) > 1) {
    loc <- findMRCA(tree, mrca_list)
  tree <- bind.tip(tree,full,where=loc)
  }else if (length(mrca_list) <= 1) {
    loc <- getParent(tree, mrca_list)
    tree <- bind.tip(tree,full,where=loc,
                     position= 0.5*tree$edge.length[which(tree$edge[,2]==loc)])
  }
  new_tree <- multi2di(tree, random = TRUE)
  }
  for (row in 1:nrow(not_found_df)) {
    full <- as.character(not_found_df[[row, "B"]])
    plot(new_tree)
    nodelabels()
    num <- readline(cat(sprintf("Where would you like to put %s Enter a node number
                              from the tree that popped up", full)) )
    num <- as.numeric(unlist(strsplit(num, ",")))
    pos <- 0.4*(tree$edge.length[which(tree$edge[,2]==num)])
    new_tree <- bind.tip(new_tree,full,where=num,
                         edge.length  = 0.4*pos)
  }
  final_tree <- multi2di(new_tree, random = TRUE)
  return(final_tree)
}
