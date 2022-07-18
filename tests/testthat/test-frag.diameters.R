

test_that("frag.diameters", {
  data(LiangAbu)
  abu.frag <- make_frag_object(cr=df.cr, fragments=fragments.info)
  abu.g <- make_cr_graph(abu.frag)
  expect_equal(as.integer(frag.diameters(abu.g)), c(24,  4,  0,  1,  1))
})

test_that("frag.diameters (with cumulative)", {
  data(LiangAbu)
  abu.frag <- make_frag_object(cr=df.cr, fragments=fragments.info)
  abu.g <- make_cr_graph(abu.frag)
  expect_equal(as.numeric(frag.diameters(abu.g, cumulative=TRUE)),
               c(1, 0.2, 0.06666667, 0.06666667, 0.03333333), tolerance = 0.00001)
})
  
  