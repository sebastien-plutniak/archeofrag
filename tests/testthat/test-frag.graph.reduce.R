test_that("preserve component number (conserving objects nr)", {
  set.seed(1)
  g1 <- frag.simul.process(1, 10, 41)
  g2 <- frag.graph.reduce(g1, 30, conserve.objects.nr = T)
  g3 <- frag.graph.reduce(g1, 40, conserve.objects.nr = T)

  expect_identical(components(g1)$no, expected = components(g2)$no)
  expect_identical(components(g1)$no, expected = components(g3)$no)
})



test_that("reduce vertice number (conserving objects nr)", {
  set.seed(1)
  g1 <- frag.simul.process(1, 10, 41)
  g2 <- frag.graph.reduce(g1, 30, conserve.objects.nr = TRUE)
  
  expect_identical(gorder(g2), 20)
})


test_that("reduce vertice number (not conserving objects nr)", {
  set.seed(1)
  g1 <- frag.simul.process(initial.layers=2, n.components = 10, vertices = 41, disturbance = 0.1)
  g2 <- frag.graph.reduce(g1, 30, conserve.objects.nr = FALSE, conserve.frag.balance = FALSE)
  
  expect_identical(gorder(g2), 12)
})




test_that("reduce vertice number (conserving objects nr, conserving fragments balance)", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=2, n.components = 10, vertices = 41, disturbance = 0.1)
  g <- frag.graph.reduce(g1, 30, conserve.objects.nr = TRUE, conserve.frag.balance = TRUE)
  
  expect_equal(gorder(g), 33)
})


