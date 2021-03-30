
frag.get.layers <- function(graph, layer.attr, sel.layers){
  if(! is.igraph(graph))  stop("Not a graph object")
  if( ! is.character(layer.attr))  stop("The parameter 'layer.attr' requires a character value.")
  if(is.null(vertex_attr(graph, layer.attr)))   stop("The parameter 'layer.attr' is required.")
  if( ! layer.attr %in% names(vertex_attr(graph)) ){
    stop(paste("No '", layer.attr, "' vertices attribute", sep=""))
  }
  
  layers <- vertex_attr(graph, layer.attr)
  if(sum(sel.layers %in% layers) != length(sel.layers)) stop("Some of the 'selected layers' are not in the 'layers' vector.")
  
  g.list <- lapply(sel.layers,
                   function(x) induced_subgraph(graph, V(graph)[layers == x] ))
  names(g.list) <- sel.layers
  g.list
}
