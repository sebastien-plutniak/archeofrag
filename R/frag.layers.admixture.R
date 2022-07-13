frag.layers.admixture <- function(graph, layer.attr){
  # output : value [0;1]. 0 = "unmixed layers", 1 = "highly mixed layers"
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # extract the user-defined layer attribute and reintegrate it as a vertices attribute named "layer":
  layers <- igraph::vertex_attr(graph, layer.attr)
  igraph::V(graph)$layer <- layers
  layers <- unique(layers)
  
  # Conditionnal tests in function of the number of layers:
  if(length(layers) < 2) stop("At least two different layers are required.")
  
  if(length(layers) == 2){
    if(is.null(igraph::E(graph)$weight)) stop("The edges must be weighted (using the 'frag.edges.weighting' function).")
    results <- c(admixture = 1 - sum(frag.layers.cohesion(graph, "layer")))
    return(results)
  } else { # if length(layers) > 2
    pairs <- utils::combn(layers, 2) 
    warning("More than 2 layers: the 'frag.edges.weighting' function has been applied to each pair of layers.")
    results <- sapply(1:ncol(pairs), function(x){
      gsub <- frag.get.layers.pair(graph, layer.attr, c(pairs[1, x], pairs[2, x]))
      gsub <- frag.edges.weighting(gsub, layer.attr)
      1 - sum(frag.layers.cohesion(gsub, layer.attr))
    })
    names(results) <- apply(pairs, 2, function(x) paste(x, collapse = "/"))
    results
  } 
}
