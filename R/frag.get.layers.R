
frag.get.layers <- function(graph, layer.attr, sel.layers){
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # function:
  layers <- igraph::vertex_attr(graph, layer.attr)
  if(sum(sel.layers %in% layers) != length(sel.layers)) stop("Some of the 'selected layers' are not in the 'layers' vector.")
  
  g.list <- lapply(sel.layers,
                   function(x) igraph::induced_subgraph(graph, V(graph)[layers == x] ))
  names(g.list) <- sel.layers
  g.list
}
