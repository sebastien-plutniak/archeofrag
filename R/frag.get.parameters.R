

frag.get.parameters <- function(graph, layer.attr, verbose = TRUE){
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # retrieve and format 'the layer' attribute:
  igraph::V(graph)$layer <- igraph::vertex_attr(graph, layer.attr)
  
  # add edge weight attribute is absent (to avoid an issue with the as_graphnel function):
  if(is.null(igraph::edge_attr(graph, "weight"))){
    igraph::E(graph)$weight <- 1
  }
  
  # test if there are two layers. If true, compute balance, components.balance, disturbance. Else return NAs
  if(length(unique(igraph::V(graph)$layer)) == 2) {
      # fragments balance ----
      balance <- .fragments.balance(graph)
      
      # components balance ----
      compo.balance <- .components.balance(graph)
      
      # estimated disturbance ----
      # proportion of fragments which might have move, determined from the components with fragments from 2 spatial units unequally represented:
      g.list <- frag.get.layers.pair(graph, "layer", unique(igraph::V(graph)$layer), mixed.components.only = TRUE, verbose = verbose)
      disturbance <- 0
      if(! is.null(g.list)){
        g.list <- igraph::decompose(g.list)
        g.list <- sapply(g.list, function(x) 
          table(factor(igraph::V(x)$layer, levels = unique(igraph::V(graph)$layer))) )
        # replace the count of the more represented layer in each component by NA:
        g.list <- apply(g.list, 2, function(x){ x[order(x)][2] <- NA ; x })
        # sum of the count of vertices for the less represented layer in each component:
        disturbance <- sum(g.list, na.rm = TRUE) / igraph::gorder(graph)
        disturbance <- round(disturbance, 2)
      } # end is.null(g.list)
  } else {
    balance <- NA
    disturbance <- NA
    compo.balance <- NA
    if(verbose) warning("The graph does not have two spatial units: disturbance, component balance, and fragment balance values are not computed.")
  }
  
  # aggregation factor ----
  # degree of aggregation of the edges on the components:
  aggreg.factor <- 1 - 1/(1 + stats::sd(sapply(igraph::decompose(graph), igraph::gsize)))
  aggreg.factor <- round(aggreg.factor, 2)
  
  # planarity  ---- 
  # only if the RBGL package is installed
  is.planar <- NA
  if ( requireNamespace("RBGL", quietly=TRUE)  ) {
    is.planar <- RBGL::boyerMyrvoldPlanarityTest(igraph::as_graphnel(graph))
  } else if(verbose) {
    message("The RBGL package is not installed, the `planarity` value cannot be determinated and returned as NA")
  }
  # list results:
  res <- list("n.components" = igraph::components(graph)$no,
              "vertices" =  igraph::gorder(graph),
              "edges" = igraph::gsize(graph),
              "balance" = balance,
              "components.balance" = compo.balance,
              "disturbance" = disturbance, 
              "aggreg.factor" = aggreg.factor,
              "planar" = is.planar,
              "edge.weights.sum" = sum(igraph::E(graph)$weight),
              "edge.weights.median" = stats::median(igraph::E(graph)$weight),
              "edge.weights.median.abs.dev." = stats::mad(igraph::E(graph)$weight)
              )
  # format and return results:
  lapply(res, c, use.names = FALSE)
}
