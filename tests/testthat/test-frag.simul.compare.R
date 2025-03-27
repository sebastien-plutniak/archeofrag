

test_that("frag.simul.compare works", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15, verbose=FALSE)
  res <- frag.simul.compare(g, layer.attr="layer", iter=60, verbose=FALSE)
  expect_equal(nrow(res[[1]]), 60)
  expect_equal(nrow(res[[2]]), 60)
  
  if (isNamespaceLoaded("RBGL")) { #  due to issues with RBGL on Win-build
    expect_equal(res[[3]]$`Obs. value/H1`, c("lower", "within", "within", "within", "within", "within", "within", "lower", "lower", "lower"))
    expect_equal(res[[3]]$`p.value`, c("0.4",  "0.38", "0.39", "0.36", "0.08", "0.01", "0", "0.57", "0.25", "0.24"))
  } else {
    expect_equal(res[[3]]$`Obs. value/H1`, c("lower", "within", "within", "within", "within", "within", "within", "lower", "lower", "lower"))
    expect_equal(res[[3]]$`p.value`, c("0.45",  "0.38", "0.39", "0.36", "0.09", "0.02", "0", "0.55", "0.25", "0.23"))
  }
})

