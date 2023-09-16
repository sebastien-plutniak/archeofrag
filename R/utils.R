

.check.frag.graph <- function(graph){
  if(! igraph::is_igraph(graph)) stop("Not a graph object")
  if(igraph::is_directed(graph)) stop("The 'graph' parameter requires an undirected igraph object.")
  if(! igraph::is_simple(graph)) stop("Loops and multiple edges are not allowed.")
}

.check.layer.argument <- function(graph, layer.attr){
  if(is.null(igraph::vertex_attr(graph, layer.attr)))   stop("'layer.attr' is missing or does not correspond to a vertex attribute of the graph.")
  if(is.null(igraph::vertex_attr(graph, layer.attr)))   stop("The parameter 'layer.attr' is required.")
  if (is.null(layer.attr))  stop("No 'layer.attr' argument")
  if( ! is.character(layer.attr))  stop("The parameter 'layer.attr' requires a character value.")
  if( ! layer.attr %in% names(igraph::vertex_attr(graph)) ){
    stop(paste("There is no '", layer.attr, "' vertices attribute.", sep=""))
  }
}