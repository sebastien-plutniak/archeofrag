frag.path.lengths <- function(graph, cumulative=FALSE){
  # tests:
  .check.frag.graph(graph)
  # main function:  
  if(! is.logical(cumulative)) stop("The 'cumulative' parameter requires a logical value.")
  
  path.vector <- igraph::distance_table(graph, directed = FALSE)$res
  
  if(cumulative){
    path.vector <- path.vector / max(path.vector)
  }
  path.vector
}
