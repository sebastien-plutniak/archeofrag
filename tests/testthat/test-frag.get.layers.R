test_that("extraction of a layer works", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=1, 
                          n.components=22,
                          vertices=60,
                          disturbance=0.1)
  g1 <- frag.get.layers(g, "layer", 1)
  expect_equal(igraph::gorder(g1[[1]]), 31)
  expect_equal(igraph::gsize(g1[[1]]), 18)
})
