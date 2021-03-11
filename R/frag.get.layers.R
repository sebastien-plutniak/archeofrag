
#'	@export
frag.get.layers <- function(graph, layer.attr, sel.layers){
  if(! is.igraph(graph))  stop("Not a graph object")
  if(! is.character(layer.attr) )  stop("'layer.attr' invalid")
  
  layers <- vertex_attr(graph, layer.attr)
  if(sum(sel.layers %in% layers) != length(sel.layers)) stop("Some of the 'selected layers' are not in the 'layers' vector.")
  
  g.list <- lapply(sel.layers,
                   function(x) induced_subgraph(graph, V(graph)[layers == x] ))
  names(g.list) <- sel.layers
  g.list
}
