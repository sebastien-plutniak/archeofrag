.randomly.delete.edges <- function(graph, value, remove.vertices){ 
  set.seed(10)  # reset the random value to ensure that the same series of edges is remove
  n.edges <- round(igraph::gsize(graph) * value)
  graph <- igraph::delete_edges(graph, sample(igraph::E(graph), n.edges))
  if(remove.vertices){
    graph <- igraph::delete_vertices(graph, igraph::degree(graph)==0)
  }
  graph
}

frag.observer.failure <- function(graph, likelihood, remove.vertices=FALSE){
  # output: a liste of altered graphs with edges (and vertices) removed
  # tests:
  .check.frag.graph(graph)
  # main function:  
  if(! is.numeric(likelihood)){
    stop("The 'likelood' parameter requires a numerical value.")
  } else  if(sum(likelihood < 0, likelihood > 1)){
    stop("The 'likelihood' values must range in [0,1].")
  }
  # apply the subsection and return results:
  lapply(likelihood, function(value) .randomly.delete.edges(graph, value, remove.vertices))
}
