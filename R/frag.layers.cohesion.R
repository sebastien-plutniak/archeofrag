#'	@export
frag.layers.cohesion <- function(graph, layer.attr){
  # output : value [0;1].
  if(! is.igraph(graph)) stop("Not a graph object")
  if(is.null(E(graph)$weight)) stop("The edge must be weighted with the 'frag.edges.weighting' function")
  if(is.null(vertex_attr(graph, layer.attr)))   stop("'layer.attr' invalid")
  if(! is.character(layer.attr) )  stop("'layer.attr' invalid")
  
  layers <- vertex_attr(graph, name = layer.attr)
  V(graph)$layers <- layers
  layers <- unique(layers)
  
  if(length(layers) != 2)  stop("Two layers are required.")
  
  v1 <- V(graph)$layer == layers[1]
  v2 <- V(graph)$layer == layers[2]
  
  g1 <-  subgraph.edges(graph, E(graph)[ V(graph)[v1] %--% V(graph)[v1] ])
  g2 <-  subgraph.edges(graph, E(graph)[ V(graph)[v2] %--% V(graph)[v2] ])
  g12 <- subgraph.edges(graph, E(graph)[ V(graph)[v1] %--% V(graph)[v2] ])
  
  # # Sums 
  res1 <-  sum(v1, E(g1)$weight) / sum(gorder(graph), E(graph)$weight)
  res2 <-  sum(v2, E(g2)$weight) / sum(gorder(graph), E(graph)$weight)
  res <- c(res1, res2)
  names(res) <- layers
  res[order(names(res))]
}
