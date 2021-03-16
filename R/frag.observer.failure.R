.randomly.delete.edges <- function(graph, value){ 
  set.seed(10)  # reset the random value to ensure the same serie of edges is remove
  n.edges <- round(gsize(graph) * value)
  delete_edges(graph, sample(E(graph), n.edges))
}

frag.observer.failure <- function(graph, likelihoods){
  # output: a liste of altered graphs with edges removed
  if(! is.igraph(graph))  stop("Not a graph object")
  if(sum(likelihoods < 0, likelihoods > 1)){
    stop("missed.relations values must range in [0,1].")
  }
  # apply the subsection and return results:
  lapply(likelihoods, function(value) .randomly.delete.edges(graph, value))
}
