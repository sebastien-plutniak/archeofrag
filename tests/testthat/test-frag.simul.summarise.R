
test_that("parameters of a constrained generated graph with 1 initial layer are correct", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  res <- frag.simul.compare(g, layer.attr="layer", iter=30, summarise=FALSE)
  res <- frag.simul.summarise(g, layer.attr="layer", res.h1=res[[1]], res.h2=res[[2]])
  expect_equal(as.character(res$p.value), 
               c("0.02", "0.06", "0.45", "0.9",  "0.98", "0.05", "0.07"))
})
