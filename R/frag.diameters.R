
#'	@export
frag.diameters <- function(graph, cumulative=FALSE) 
{
  if( ! is.igraph(graph)) {
    stop("Not a graph object")
  }
  if( ! is.null(graph_attr(graph, "frag_type")) ) {
    if(  graph_attr(graph, "frag_type") == "connection and similarity" ){
      warning("Diameter distribution is meaningless for 'connection and similarity' graph type")
    }
  }
  g.list <- decompose(graph)
  diameter.v <- sapply(g.list, diameter, weights=NULL, directed=FALSE,  unconnected = TRUE)
  diameter.v <-  hist(diameter.v, -1:max(diameter.v), plot = FALSE)$density 
  if(cumulative){
    diameter.v <- rev(cumsum(rev(diameter.v)))
  }
  diameter.v
}
