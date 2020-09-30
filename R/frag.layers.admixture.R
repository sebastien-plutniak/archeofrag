#'	@export
frag.layers.admixture <- function(graph, layer.attr){
  # output : value [0;1]. 0 = "unmixed layers", 1 = "highly mixed layers"
  if(! is.igraph(graph))  stop("Not a graph object")
  if(! is.character(layer.attr)) stop("'layer.attr' invalid")

  layers <- vertex_attr(graph, layer.attr)
  
  if(length(unique(layers)) > 2) stop("The graph must have no more than two layers.")
  V(graph)$layer <- layers
  
  1 - sum(frag.layers.cohesion(graph, "layer"))
}
