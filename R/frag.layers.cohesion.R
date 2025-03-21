.cohesion.for.two.layers <- function(g, layers){
  v1 <- igraph::V(g)$layer == layers[1]
  v2 <- igraph::V(g)$layer == layers[2]
  
  # extract three subgraphs:
  g1 <-  igraph::subgraph_from_edges(g, igraph::E(g)[ igraph::V(g)[v1] %--% igraph::V(g)[v1] ])
  g2 <-  igraph::subgraph_from_edges(g, igraph::E(g)[ igraph::V(g)[v2] %--% igraph::V(g)[v2] ])
  g12 <- igraph::subgraph_from_edges(g, igraph::E(g)[ igraph::V(g)[v1] %--% igraph::V(g)[v2] ])
  
  # compute the cohesion values:
  res1 <-  sum(v1, igraph::E(g1)$weight) / sum(igraph::gorder(g), igraph::E(g)$weight)
  res2 <-  sum(v2, igraph::E(g2)$weight) / sum(igraph::gorder(g), igraph::E(g)$weight)
  
  # format and return results:
  c(res1, res2)
}

frag.layers.cohesion <- function(graph, layer.attr, morphometry=NULL, x=NULL, y=NULL, z=NULL, verbose=TRUE){
  # output : value [0;1].
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # delete singletons:
  graph <- igraph::delete_vertices(graph, igraph::degree(graph) == 0)
  
  # Test if empty graph:
  if(length(graph) == 0){
    if(verbose) warning("Empty graph.")
    return(c("cohesion1" = NA, "cohesion2" = NA))
  }
  
  # extract the user-defined layer attribute and reintegrate it as a vertices attribute named "layer":
  layers <- igraph::vertex_attr(graph, name = layer.attr)
  igraph::V(graph)$layer <- layers
  layers <- sort(unique(layers))
  # Test the number of layers :
  if(length(layers) < 2){
    if(verbose) warning("At least two different layers are required.")
    return(c("cohesion1" = NA, "cohesion2" = NA))
  }
  pairs <- utils::combn(layers, 2) 
  
  if(length(layers) == 2){
    if(verbose & is.null(igraph::E(graph)$weight)) stop("The edges must be weighted (using the 'frag.edges.weighting' function).")
    results <- .cohesion.for.two.layers(graph, layers)
    results <- matrix(results)
  } else{ # if length(layers) > 2
    if(verbose) message("More than 2 layers: the 'frag.edges.weighting' function is applied to each pair of layers.")
    
    results <- sapply(seq_len(ncol(pairs)), function(id){
      res <- c("cohesion1" = NA, "cohesion2" = NA)
      gsub <- frag.get.layers.pair(graph, layer.attr, c(pairs[1, id], pairs[2, id]), verbose = verbose)
      if(is.null(gsub)) return(res)
      
      if(length(unique(igraph::V(gsub)$layer)) == 2){
        gsub <- frag.edges.weighting(gsub, layer.attr, morphometry, x, y, z, verbose = verbose)
        res <- .cohesion.for.two.layers(gsub, c(pairs[1, id], pairs[2, id]))
      }
      res
    })
  }
  rownames(results) <- c("cohesion1", "cohesion2")
  colnames(results) <- apply(pairs, 2, function(x) paste(x, collapse = "/"))
  results <- round(results, digits = 4)
  t(results)
}
