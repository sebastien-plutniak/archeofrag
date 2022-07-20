test_that("extraction of a layer works", {
  fragments.df <- data.frame(matrix(c(1,1, 2,1, 3,1, 4,1, 5,1, 6,1, 7,2, 8,2,
                                      9,1, 10,1, 11,2, 12,2, 13,1, 14,1, 15,1,
                                      16,1, 17,2, 18,1, 19,1, 20,1, 21,1, 22,1,
                                      23,1, 24,2, 25,2, 26,2, 27,2, 28,2, 29,2,
                                      30,2, 31,2, 32,2), ncol=2, byrow=T))
  colnames(fragments.df) <- c("node", "layer")
  edges <- data.frame(matrix(c(1,2, 3,4, 4,5, 6,7, 7,8, 8,6, 9,10,
                               10,11, 11,12, 12,9, 13,14, 14,15, 13,15,
                               15,16, 14,17, 18,19, 19,20, 20,18, 19,21,
                               22,23, 23,24, 24,25, 26,27, 27,28, 28,29,
                               26,29, 29,32, 31,32, 31,30, 30,29, 31,29),
                             ncol=2, byrow=T))
  g <- make_frag_object(cr=edges, fragments=fragments.df)
  g <- make_cr_graph(g)
  
  g1 <- frag.get.layers(g, "layer", 1)
  expect_equal(igraph::gorder(g1[[1]]), 18)
  expect_equal(igraph::gsize(g1[[1]]), 13)
})
