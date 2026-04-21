set.seed(1)
g1 <- frag.simul.process(n.components=5,  vertices=20, disturbance=.1)
g2 <- frag.simul.process(n.components=10, vertices=90, disturbance=.03)
g3 <- frag.simul.process(n.components=20, vertices=50, disturbance=.4)

igraph::V(g2)$name <- paste0(igraph::V(g2)$name, "g2")
igraph::V(g3)$name <- paste0(igraph::V(g3)$name, "g3") 

igraph::V(g2)$layer <- as.character(factor(igraph::V(g2)$layer, labels = c(3, 4))) 
igraph::V(g3)$layer <- as.character(factor(igraph::V(g3)$layer, labels = c(5, 6))) 

g1234 <- igraph::disjoint_union(g1, g2) 
g1234$frag_type <- "cr" 

g1256 <- igraph::disjoint_union(g1, g3) 
g1256$frag_type <- "cr" 

cohesion.1234 <- frag.layers.cohesion(g1234, layer.attr="layer", verbose=FALSE) 
cohesion.1256 <- frag.layers.cohesion(g1256, layer.attr="layer", verbose=FALSE) 


test_that("add.math.signs works", {
  rank <- frag.cohesion.ranking(cohesion.1234, add.math.signs = TRUE) 
  
  expect_equal(names(rank), c("3", ">", "4", ">", "2", ">", "1", ""))
})



test_that("labels rank and count are correct in case 1", {
  rank <- frag.cohesion.ranking(cohesion.1234, add.math.signs = FALSE) 
  
  expect_equal(names(rank), c("3", "4", "2", "1"))
  expect_equal(c(rank, use.names=FALSE), c(3, 2, 1, 0))
})

test_that("labels ranks are different in case 2", {
  rank <- frag.cohesion.ranking(cohesion.1256, add.math.signs = FALSE) 
  
  expect_equal(names(rank), c("2", "1", "6", "5"))
  expect_equal(c(rank, use.names=FALSE), c(3, 2, 1, 0))
})
