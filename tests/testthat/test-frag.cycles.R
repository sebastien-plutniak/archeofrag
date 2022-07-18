
test_that("frag.cycles", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=2, 
                          n.components=22,
                          vertices=60,
                          disturbance=0.1)
  res <- frag.cycles(g, kmax=4, max.cycles.only=FALSE)
  expect_equal(as.integer(res), c(11, 3))
})

test_that("frag.cycles (with max.cycles.only)", {
  set.seed(1)
  g <- frag.simul.process(initial.layers=2, 
                          n.components=22,
                          vertices=60,
                          disturbance=0.1)
  res <- frag.cycles(g, kmax=4, max.cycles.only=TRUE)
  expect_equal(as.integer(res), c(4, 3))
})
