.cohesion.for.two.layers <- function(g, layers){
  v1 <- igraph::V(g)$layers == layers[1]
  v2 <- igraph::V(g)$layers == layers[2]
  
  # extract three subgraphs:
  g1 <-  igraph::subgraph.edges(g, igraph::E(g)[ igraph::V(g)[v1] %--% igraph::V(g)[v1] ])
  g2 <-  igraph::subgraph.edges(g, igraph::E(g)[ igraph::V(g)[v2] %--% igraph::V(g)[v2] ])
  g12 <- igraph::subgraph.edges(g, igraph::E(g)[ igraph::V(g)[v1] %--% igraph::V(g)[v2] ])
  
  # compute the cohesion values:
  res1 <-  sum(v1, igraph::E(g1)$weight) / sum(igraph::gorder(g), igraph::E(g)$weight)
  res2 <-  sum(v2, igraph::E(g2)$weight) / sum(igraph::gorder(g), igraph::E(g)$weight)
  
  # format and return results:
  c(res1, res2)
}

frag.layers.cohesion <- function(graph, layer.attr){
  # output : value [0;1].
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # delete singletons:
  graph <- igraph::delete_vertices(graph, degree(graph) == 0)
  
  # extract the user-defined layer attribute and reintegrate it as a vertices attribute named "layer":
  layers <- igraph::vertex_attr(graph, name = layer.attr)
  igraph::V(graph)$layers <- layers
  layers <- sort(unique(layers))
  # Conditionnal tests in function of the number of layers:
  if(length(layers) < 2){
    warning("At least two different layers are required.")
    return(c(NA, NA))
  }
  pairs <- utils::combn(layers, 2) 
  
  if(length(layers) == 2){
    if(is.null(igraph::E(graph)$weight)) stop("The edges must be weighted (using the 'frag.edges.weighting' function).")
    results <- .cohesion.for.two.layers(graph, layers)
    results <- matrix(results)
  } else{ # if length(layers) > 2
    warning("More than 2 layers: the 'frag.edges.weighting' function is applied to each pair of layers.")
    results <- sapply(1:ncol(pairs), function(x){
      gsub <- frag.get.layers.pair(graph, layer.attr, c(pairs[1, x], pairs[2, x]))
      gsub <- frag.edges.weighting(gsub, layer.attr)
      .cohesion.for.two.layers(gsub, unique(igraph::V(gsub)$layers))
    })
  }
  rownames(results) <- c("cohesion1", "cohesion2")
  colnames(results) <- apply(pairs, 2, function(x) paste(x, collapse = "/"))
  t(results)
}
