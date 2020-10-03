
#'	@export
frag.diameters <- function(graph, cumulative=FALSE) 
{
  if( ! is.igraph(graph)) stop("Not a graph object")
  if( ! is.null(graph_attr(graph, "frag_type")) ) {
    if(graph_attr(graph, "frag_type") == "connection and similarity"){
      warning("Diameter distribution is meaningless for 'connection and similarity' graph type")
    }
  }
  g.list <- decompose(graph)
  diameter.v <- sapply(g.list, diameter, weights=NA, directed=FALSE, unconnected=FALSE)
  diameter.v <- hist(diameter.v, breaks=0:max(diameter.v), plot=FALSE)$count 
  if(cumulative){
    diameter.v <- rev(cumsum(rev(diameter.v))) / sum(diameter.v)
  }
  names(diameter.v) <- 1:length(diameter.v)
  diameter.v
}

