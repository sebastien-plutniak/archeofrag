.cohesion.for.two.layers <- function(g, layers){
  v1 <- V(g)$layers == layers[1]
  v2 <- V(g)$layers == layers[2]
  
  # extract three subgraphs:
  g1 <-  subgraph.edges(g, E(g)[ V(g)[v1] %--% V(g)[v1] ])
  g2 <-  subgraph.edges(g, E(g)[ V(g)[v2] %--% V(g)[v2] ])
  g12 <- subgraph.edges(g, E(g)[ V(g)[v1] %--% V(g)[v2] ])
  
  # compute the cohesion values:
  res1 <-  sum(v1, E(g1)$weight) / sum(gorder(g), E(g)$weight)
  res2 <-  sum(v2, E(g2)$weight) / sum(gorder(g), E(g)$weight)
  
  # format and return the results:
  c(res1, res2)
}

frag.layers.cohesion <- function(graph, layer.attr){
  # output : value [0;1].
  if(! is.igraph(graph)) stop("Not a graph object")
  if(is.null(vertex_attr(graph, layer.attr)))   stop("'layer.attr' invalid")
  if(! is.character(layer.attr) )  stop("'layer.attr' invalid")
  
  # delete singletons:
  graph <- delete_vertices(graph, degree(graph) == 0)
  
  # extract the user-defined layer attribute and reintegrate it as a vertices attribute named "layer":
  layers <- vertex_attr(graph, name = layer.attr)
  V(graph)$layers <- layers
  layers <- sort(unique(layers))
  pairs <- combn(layers, 2) 
  
  # Conditionnal tests in function of the number of layers:
  if(length(layers) < 2) stop("At least two different layers are required.")
  
  if(length(layers) == 2){
    if(is.null(E(graph)$weight)) stop("The edges must be weighted (using the 'frag.edges.weighting' function).")
    results <- .cohesion.for.two.layers(graph, layers)
    results <- matrix(results)
  } else{ # if length(layers) > 2
    warning("More than 2 layers: the 'frag.edges.weighting' function has been applied to each pair of layers.")
    results <- sapply(1:ncol(pairs), function(x){
      gsub <- frag.get.layers.pair(graph, layer.attr, c(pairs[1, x], pairs[2, x]))
      gsub <- frag.edges.weighting(gsub, layer.attr)
      .cohesion.for.two.layers(gsub, unique(V(gsub)$layers))
    })
  }
  rownames(results) <- c("cohesion1", "cohesion2")
  colnames(results) <- apply(pairs, 2, function(x) paste(x, collapse = "/"))
  t(results)
}
