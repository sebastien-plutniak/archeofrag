.randomly.delete.edges <- function(graph, value){ 
  set.seed(10)  # reset the random value to ensure that the same series of edges is remove
  n.edges <- round(gsize(graph) * value)
  delete_edges(graph, sample(E(graph), n.edges))
}

frag.observer.failure <- function(graph, likelihood){
  # output: a liste of altered graphs with edges removed
  if(! is.igraph(graph))  stop("Not a graph object")
  if(! is.numeric(likelihood)){
    stop("The 'likelood' parameter requires a numerical value.")
  } else  if(sum(likelihood < 0, likelihood > 1)){
    stop("The 'likelihood' values must range in [0,1].")
  }
  # apply the subsection and return results:
  lapply(likelihood, function(value) .randomly.delete.edges(graph, value))
}
