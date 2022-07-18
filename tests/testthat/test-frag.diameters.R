

test_that("frag.diameters", {
  data(LiangAbu)
  abu.frag <- make_frag_object(cr=df.cr, fragments=fragments.info)
  abu.g <- make_cr_graph(abu.frag)
  expect_equal(as.integer(frag.diameters(g)), c(15, 6, 1))
})

test_that("frag.diameters (with cumulative)", {
  data(LiangAbu)
  abu.frag <- make_frag_object(cr=df.cr, fragments=fragments.info)
  abu.g <- make_cr_graph(abu.frag)
  expect_equal(as.numeric(frag.diameters(g, cumulative=TRUE)),
               c(1, 0.31818182, 0.04545455))
})
  
  