test_that("random removal of edges works", {
  set.seed(1)
  g1 <- frag.simul.process(initial.layers=1, 
                          n.components=22,
                          vertices=60,
                          edges=55,
                          disturbance=0.1)
  g2 <- frag.observer.failure(g1, c(0.2, 0.3))
  expect_equal(igraph::gsize(g2[[1]]), 44)
  expect_equal(igraph::gsize(g2[[2]]), 39)
})
