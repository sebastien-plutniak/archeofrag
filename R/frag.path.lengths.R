
#'	@export
frag.path.lengths <- function(graph, cumulative=FALSE){
  if(! is.igraph(graph)) stop("Not a graph object")
  
  path.vector <- distance_table(graph, directed = FALSE)$res
  
  if(cumulative){
    path.vector <- path.vector / max(path.vector)
  }
  path.vector
}
