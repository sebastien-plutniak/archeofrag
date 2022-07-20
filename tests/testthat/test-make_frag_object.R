

test_that("make_frag_object (only connection relationships)", {
  cr.df <- matrix(c(1,2, 1,3, 2,3, 4,5, 4,6, 7,8), ncol=2, byrow=TRUE)
  sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
  fragments.df <- data.frame(1:13, letters[1:13]) 
  
  f.obj <- make_frag_object(cr=cr.df, fragments=fragments.df)
  expect_equal(class(f.obj)[1], "Frag.object")
  expect_equal(typeof(f.obj), "S4")
})

test_that("make_frag_object (connection and similarity relationships)", {
  cr.df <- matrix(c(1,2, 1,3, 2,3, 4,5, 4,6, 7,8), ncol=2, byrow=TRUE)
  sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
  fragments.df <- data.frame(1:13, letters[1:13]) 
  
  f.obj <- make_frag_object(cr=cr.df, sr=sr.df, fragments=fragments.df)
  expect_equal(class(f.obj)[1], "Frag.object")
  expect_equal(typeof(f.obj), "S4")
})

test_that("make_sr_graph works", {
  sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
  fragments.df <- data.frame(1:13, letters[1:13]) 
  
  f.obj <- make_frag_object(sr=sr.df, fragments=fragments.df)
  g <- make_sr_graph(f.obj)
  
  expect_equal(class(g), "igraph")
  expect_equal(igraph::gorder(g), 6)
  expect_equal(igraph::gsize(g), 6)
})


test_that("make_crsr_graph works", {
  cr.df <- matrix(c(1,2, 1,3, 2,3, 4,5, 4,6, 7,8), ncol=2, byrow=TRUE)
  sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
  fragments.df <- data.frame(1:13, letters[1:13]) 
  
  f.obj <- make_frag_object(cr=cr.df, sr=sr.df, fragments=fragments.df)
  g <- make_crsr_graph(f.obj)
  
  expect_equal(class(g), "igraph")
  expect_equal(igraph::gorder(g), 13)
  expect_equal(igraph::gsize(g), 17)
})


test_that("make_cr_graph works", {
  fragments.df <- data.frame(matrix(c(1,1, 2,1, 3,1, 4,1, 5,1, 6,1, 7,2, 8,2,
                                      9,1, 10,1, 11,2, 12,2, 13,1, 14,3, 15,1,
                                      16,1, 17,2, 18,1, 19,1, 20,1, 21,1, 22,1,
                                      23,3, 24,3, 25,2, 26,2, 27,2, 28,2, 29,2,
                                      30,2, 31,2, 32,2), ncol=2, byrow=T))
  colnames(fragments.df) <- c("node", "layer")
  edges <- data.frame(matrix(c(1,2, 3,4, 4,5, 6,7, 7,8, 8,6, 9,10,
                               10,11, 11,12, 12,9, 13,14, 14,15, 13,15,
                               15,16, 14,17, 18,19, 19,20, 20,18, 19,21,
                               22,23, 23,24, 24,25, 26,27, 27,28, 28,29,
                               26,29, 29,32, 31,32, 31,30, 30,29, 31,29),
                             ncol=2, byrow=T))
  g <- make_frag_object(cr=edges, fragments=fragments.df)
  g <- make_cr_graph(g)
  
  expect_equal(class(g), "igraph")
  expect_equal(igraph::gorder(g), 32)
  expect_equal(igraph::gsize(g), 31)
})  




