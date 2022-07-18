test_that("retrieved parameters are correct", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=1, 
                          n.components=22,
                          vertices=60,
                          edges=55,
                          disturbance=0.1)
  expect_equal(frag.get.parameters(g, "layer")$n.components, 22)
  expect_equal(frag.get.parameters(g, "layer")$vertices, 60)
  expect_equal(frag.get.parameters(g, "layer")$edges, 55)
  expect_equal(frag.get.parameters(g, "layer")$balance, 0.52)
  expect_equal(frag.get.parameters(g, "layer")$components.balance, 0.5)
  expect_equal(frag.get.parameters(g, "layer")$disturbance, 0.1)
  expect_equal(frag.get.parameters(g, "layer")$aggreg.factor, 0.6)
  expect_equal(frag.get.parameters(g, "layer")$planar, TRUE)
})
