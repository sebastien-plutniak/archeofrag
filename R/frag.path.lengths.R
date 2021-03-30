frag.path.lengths <- function(graph, cumulative=FALSE){
  if(! is.igraph(graph)) stop("Not a graph object")
  if(! is.logical(cumulative)) stop("The 'cumulative' parameter requires a logical value.")
  
  path.vector <- distance_table(graph, directed = FALSE)$res
  
  if(cumulative){
    path.vector <- path.vector / max(path.vector)
  }
  path.vector
}
