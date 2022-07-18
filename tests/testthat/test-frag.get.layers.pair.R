
test_that("get layers pair", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  igraph::V(g)$layers <- sample(c(1, 2, 3), igraph::gorder(g), replace=T)
  gsub <- frag.get.layers.pair(g, "layers", sel.layers=c("1", "2"))
  expect_equal(igraph::gorder(gsub), 23)
})  

test_that("get layers pair (use size.mini)", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  igraph::V(g)$layers <- sample(c(1, 2, 3), igraph::gorder(g), replace=T)
  gsub <- frag.get.layers.pair(g, "layers", sel.layers=c("1", "2"), size.mini=3)
  expect_equal(igraph::gorder(gsub), 5)
})  

test_that("get layers pair (use mixed.components.only)", {
  set.seed(1)
  g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
  igraph::V(g)$layers <- sample(c(1, 2, 3), igraph::gorder(g), replace=T)
  gsub <- frag.get.layers.pair(g, "layers", sel.layers=c("2", "3"), mixed.components.only=T)
  expect_equal(igraph::gorder(gsub), 20)
})  

