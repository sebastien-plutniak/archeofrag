

test_that("frag.simul.compare works", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  g <- frag.edges.weighting(g, layer.attr="layer")
  res <-frag.simul.compare(g, layer.attr="layer", iter=30)
  expect_equal(nrow(res[[1]]), 30)
  expect_equal(nrow(res[[2]]), 30)
  expect_equal(res[[3]]$p.value, c("0.02", "0.06", "0.45", "0.9", "0.98", "0.05", "0.07"))
  
})
