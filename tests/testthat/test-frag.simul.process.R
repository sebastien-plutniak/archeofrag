
# ONE INITIAL LAYER ####

test_that("parameters of a constrained generated graph with 1 initial layer are correct", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=1, 
                          n.components=22,
                          vertices=60,
                          edges=55,
                          disturbance=0.1)
  expect_equal(igraph::gsize(g), 55)
  expect_equal(igraph::gorder(g), 60)
  expect_equal(igraph::components(g)$no, 22)
})


test_that("number of not constrained vertices is correct", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=1, 
                          n.components=22,
                          edges=55,
                          disturbance=0.1)
  expect_equal(igraph::gorder(g), 67)
})

test_that("cohesion values of a graph with 1 initial layer are correct", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=1, 
                          n.components=20,
                          vertices=55,
                          edges=45,
                          disturbance=0.1)
  expect_equal(frag.layers.cohesion(g, "layer")[1], 0.5123021, tolerance=.00001)
  expect_equal(frag.layers.cohesion(g, "layer")[2], 0.4180882, tolerance=.00001)
  expect_equal(as.numeric(frag.layers.admixture(g, "layer")), 0.0696097)
})



# TWO INITIAL LAYERS ####

test_that("parameters of a generated graph with 2 initial layers are correct", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=2, 
                          n.components=20,
                          vertices=62,
                          disturbance=0.1)
  expect_equal(igraph::gsize(g), 55)
  expect_equal(igraph::gorder(g), 62)
  expect_equal(igraph::components(g)$no, 20)
})


test_that("admixture and cohesion values of a graph with 2 initial layers are correct", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=2, 
                          n.components=21,
                          vertices=55,
                          disturbance=0.15)
  expect_equal(frag.layers.cohesion(g, "layer")[1], 0.4910982, tolerance=.00001)
  expect_equal(frag.layers.cohesion(g, "layer")[2], 0.4151224, tolerance=.00001)
  expect_equal(as.numeric(frag.layers.admixture(g, "layer")), 0.09377934, tolerance=.00001)
})

test_that("the optional asymmetric.transport.from parameter works", {
  set.seed(1)
  g1 <- frag.simul.process(initial.layers=2, 
                          n.components=21,
                          vertices=55,
                          disturbance=0.15,
                          asymmetric.transport.from=1)
  g2 <- frag.simul.process(initial.layers=2, 
                          n.components=21,
                          vertices=55,
                          disturbance=0.15,
                          asymmetric.transport.from=2)
  expect_equal(as.numeric(table(igraph::V(g1)$layer)[1]), 20)
  expect_equal(as.numeric(table(igraph::V(g2)$layer)[1]), 36)
})


test_that("the optional from.observed.graph parameter works", {
  set.seed(1)
  g1 <- frag.simul.process(initial.layers=2, 
                           n.components=21,
                           vertices=55,
                           disturbance=0.15,
                           asymmetric.transport.from=1)
  g2 <- frag.simul.process(from.observed.graph=g1, observed.layer.attr="layer")
  expect_equal(class(g2), "igraph")
  expect_equal(igraph::gorder(g1), igraph::gorder(g2))
})
