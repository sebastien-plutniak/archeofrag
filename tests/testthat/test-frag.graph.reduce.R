test_that("preserve component number (conserving objects nr)", {
  set.seed(1)
  g1 <- frag.simul.process(1, 10, 41)
  g2 <- frag.graph.reduce(g1, n.frag.to.remove =30, conserve.objects.nr = TRUE)
  g3 <- frag.graph.reduce(g1, n.frag.to.remove = 40, conserve.objects.nr = TRUE)

  expect_identical(components(g1)$no, expected = components(g2)$no)
  expect_identical(components(g1)$no, expected = components(g3)$no)
})



test_that("reduce vertice number (conserving objects nr)", {
  set.seed(1)
  g1 <- frag.simul.process(1, 10, 41)
  g2 <- frag.graph.reduce(g1, n.frag.to.remove = 30, conserve.objects.nr = TRUE)
  
  expect_identical(gorder(g2), 20)
})


test_that("reduce vertice number (not conserving objects nr)", {
  set.seed(1)
  g1 <- frag.simul.process(initial.layers=2, n.components = 10, vertices = 41, disturbance = 0.1)
  g2 <- frag.graph.reduce(g1, n.frag.to.remove = 30, conserve.objects.nr = FALSE, conserve.fragments.balance = FALSE, conserve.inter.units.connection = FALSE)
  
  expect_identical(gorder(g2), 11)
})


test_that("reduce vertice number (conserving objects nr, conserving fragments balance)", {
  set.seed(1)
  g1 <- frag.simul.process(initial.layers=2, n.components = 10, vertices = 51, disturbance = 0.15)
  g2 <- frag.graph.reduce(g1, n.frag.to.remove = 20, conserve.objects.nr = TRUE, conserve.fragments.balance = TRUE, conserve.inter.units.connection = FALSE)

  expect_equal(frag.get.parameters(g1, "layer")$balance,
               frag.get.parameters(g2, "layer")$balance,
               tolerance = 0.1)
    
  expect_equal(gorder(g2), 31)
  expect_lt(frag.relations.by.layers(g2, "layer")[2, 1],
            frag.relations.by.layers(g1,  "layer")[2, 1])
})


test_that("reduce vertice number (conserving fragments balance and inter-units connection)", {
  set.seed(1)
  g1 <- frag.simul.process(initial.layers=2, n.components = 11, vertices = 61, disturbance = 0.15)
  g2 <- frag.graph.reduce(g1, n.frag.to.remove = 30, conserve.objects.nr = FALSE, conserve.fragments.balance = TRUE, conserve.inter.units.connection = TRUE)
  
  expect_equal(frag.get.parameters(g1, "layer")$balance,
               frag.get.parameters(g2, "layer")$balance,
               tolerance = 0.08)
  
  expect_equal(gorder(g2), 32)
  expect_equal(frag.relations.by.layers(g2,  "layer")[2, 1], 14)
})

