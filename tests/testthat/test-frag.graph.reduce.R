test_that("preserve component number (objects nr preservation)", {
  set.seed(1)
  g1 <- frag.simul.process(1, 10, 41)
  g2 <- frag.graph.reduce(g1, 30, conserve.objects.nr = T)
  g3 <- frag.graph.reduce(g1, 40, conserve.objects.nr = T)

  expect_identical(components(g1)$no, expected = components(g2)$no)
  expect_identical(components(g1)$no, expected = components(g3)$no)
})



test_that("reduce vertice size (objects nr preservation)", {
  set.seed(1)
  g1 <- frag.simul.process(1, 10, 41)
  g2 <- frag.graph.reduce(g1, 30, conserve.objects.nr = TRUE)
  
  expect_identical(gorder(g2), 20)
})


test_that("reduce vertice size (no objects nr preservation)", {
  set.seed(1)
  g1 <- frag.simul.process(1, 10, 41)
  g2 <- frag.graph.reduce(g1, 30, conserve.objects.nr = FALSE)
  
  expect_identical(gorder(g2), 11)
})


