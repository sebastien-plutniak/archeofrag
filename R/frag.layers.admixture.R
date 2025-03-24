frag.layers.admixture <- function(graph, layer.attr, morphometry=NULL, x=NULL, y=NULL, z=NULL, verbose=TRUE){
  # output : value [0;1]. 0 = "unmixed layers", 1 = "highly mixed layers"
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # extract the user-defined layer attribute and reintegrate it as a vertices attribute named "layer":
  layers <- igraph::vertex_attr(graph, layer.attr)
  igraph::V(graph)$layer <- layers
  layers <- sort(unique(layers))
  
  # Test if empty graph:
  if(length(graph) == 0){
    if(verbose) warning("Empty graph.")
    return(NA)
  }
  
  # Test the number of layers:
  if(length(layers) < 2){
    if(verbose) warning("At least two different layers are required.")
    return(NA)
  }
  
  if(length(layers) == 2){
    if(verbose & is.null(igraph::E(graph)$weight)) stop("The edges must be weighted (using the 'frag.edges.weighting' function).")
    results <- c(admixture = 1 - sum(frag.layers.cohesion(graph, "layer")))
    return(results)
  } else { # if length(layers) > 2
    pairs <- utils::combn(layers, 2) 
    message("More than 2 layers: the 'frag.edges.weighting' function is applied to each pair of layers.")
    results <- sapply(seq_len(ncol(pairs)), function(id){
      gsub <- frag.get.layers.pair(graph, layer.attr, c(pairs[1, id], pairs[2, id]), verbose = T)
      if(is.null(gsub)){
        NA
      } else if(length(unique(V(gsub)$layer)) == 2){
        gsub <- frag.edges.weighting(gsub, layer.attr, morphometry, x, y, z)
        1 - sum(frag.layers.cohesion(gsub, layer.attr))
      } else {
        NA
      }
      
    })
    names(results) <- apply(pairs, 2, function(x) paste(x, collapse = "/"))
    results
  }
}
