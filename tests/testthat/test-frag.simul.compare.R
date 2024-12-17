

test_that("frag.simul.compare works", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  g <- frag.edges.weighting(g, layer.attr="layer")
  res <- frag.simul.compare(g, layer.attr="layer", iter=60)
  expect_equal(nrow(res[[1]]), 60)
  expect_equal(nrow(res[[2]]), 60)
  
  if ( requireNamespace("RBGL", quietly=TRUE)  ) { #  due to issues with RBGL on Win-build 
    expect_equal(res[[3]]$`Obs. value/H1`, c("lower", "lower", "within", "within", "within", "within", "within", "within"))
    expect_equal(res[[3]]$`p.value`, c("0.4",  "0.57", "0.38", "0.39", "0.36", "0.08", "0.01", "0"))
  } else {
    expect_equal(res[[3]]$`Obs. value/H1`, c("lower", "lower", "within", "within", "within", "within", "within", "within"))
    expect_equal(res[[3]]$`p.value`, c("0.45", "0.55", "0.38", "0.39", "0.36", "0.09", "0.02", "0"))
  }
  
})


