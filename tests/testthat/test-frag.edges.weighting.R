test_that("weighting  based only on the structure of the graph", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  g <- frag.edges.weighting(g , "layer")
  expect_equal(sum(igraph::E(g)$weight), 73.60564,  tolerance=.00001)
})


test_that("weighting with morphometric and spatial parameters", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  igraph::V(g)$morpho <- sample(1:20, 50, replace=TRUE)
  igraph::V(g)$x <- sample(1:100, 50, replace=TRUE)
  igraph::V(g)$y <- sample(1:100, 50, replace=TRUE)
  igraph::V(g)$z <- sample(1:100, 50, replace=TRUE)
  g <- frag.edges.weighting(g, "layer", "morpho", "x", "y", "z")
  expect_equal(sum(igraph::E(g)$weight), 34.66064,  tolerance=.00001)
  
})
