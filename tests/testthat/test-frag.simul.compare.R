

test_that("frag.simul.compare works", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  g <- frag.edges.weighting(g, layer.attr="layer")
  res <- frag.simul.compare(g, layer.attr="layer", iter=60)
  expect_equal(nrow(res[[1]]), 60)
  expect_equal(nrow(res[[2]]), 60)
  expect_equal(res[[3]]$`Obs. value/H1`, c("lower", "lower", "within", "within", "within", "within", "within", "within"))
})
