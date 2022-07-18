

test_that("the count of edges by layer is correct", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=1, 
                          n.components=20,
                          vertices=55,
                          edges=45,
                          disturbance=0.1)
  expect_equal(frag.relations.by.layers(g, "layer")[1,1], 19)
  expect_equal(frag.relations.by.layers(g, "layer")[2,1], 9)
  expect_equal(frag.relations.by.layers(g, "layer")[2,2], 17)
})

