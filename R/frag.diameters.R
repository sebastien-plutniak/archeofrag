
#
frag.diameters <- function(graph, cumulative=FALSE){
  # tests:
  .check.frag.graph(graph)
  
  if( ! is.null(igraph::graph_attr(graph, "frag_type")) ) {
    if(igraph::graph_attr(graph, "frag_type") == "connection and similarity"){
      warning("Diameter distribution is meaningless for 'connection and similarity' fragmentation graphs.")
    }
  }
  if(! is.logical(cumulative)) stop("The 'cumulative' parameter must be logical.")
  
  # main body:
  g.list <- igraph::decompose(graph)
  diameter.v <- sapply(g.list, igraph::diameter, weights=NA, directed=FALSE, unconnected=FALSE)
  diameter.v <- graphics::hist(diameter.v, breaks=0:max(diameter.v), plot=FALSE)$count 
  if(cumulative){
    diameter.v <- rev(cumsum(rev(diameter.v))) / sum(diameter.v)
  }
  names(diameter.v) <- 1:length(diameter.v)
  diameter.v
}

