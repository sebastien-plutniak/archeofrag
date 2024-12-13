

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
  
  # test if there are two layers:
  if(verbose & length(unique(igraph::V(graph)$layer)) != 2) {
    warning("The graph does not have two layers, disturbance and balance values will be meaningless.")
  }
  
  # balance: proportion of non-disturbed fragments in the two layers:
  v1 <- igraph::V(graph)[igraph::V(graph)$layer == unique(igraph::V(graph)$layer)[1]]
  v2 <- igraph::V(graph)[igraph::V(graph)$layer == unique(igraph::V(graph)$layer)[2]]
  subgraph <- igraph::subgraph_from_edges(graph, igraph::E(graph)[ ! v1 %--% v2 ])
  balance <- (table(igraph::V(subgraph)$layer) / sum(table(igraph::V(subgraph)$layer)) )[1]
  balance <- round(balance, 2)
  
  # components balance:
  compo.balance <- sapply(igraph::decompose(subgraph), 
                          function(x) igraph::V(x)$layer[1])
  compo.balance <- round(table(compo.balance)[1] / sum(table(compo.balance)), 2) 
  
  # disturbance: number of pieces which might have move:
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
  } 
  
  # degree of aggregation of the edges on the components:
  aggreg.factor <- 1 - 1/(1 + stats::sd(sapply(igraph::decompose(graph), igraph::gsize)))
  aggreg.factor <- round(aggreg.factor, 2)
  
  # planarity (if the RBGL package is installed)
  is.planar <- NA
  if ( requireNamespace("RBGL", quietly=TRUE)  ) {
    is.planar <- RBGL::boyerMyrvoldPlanarityTest(igraph::as_graphnel(graph))
  } else if(verbose) {
    warning("The RBGL package is not installed, the `planarity` value is indeterminated and set to NA")
  }
  # list results:
  res <- list("n.components" = igraph::components(graph)$no,
              "vertices" =  igraph::gorder(graph),
              "edges" = igraph::gsize(graph),
              "balance" = balance,
              "components.balance" = compo.balance,
              "disturbance" = disturbance, 
              "aggreg.factor" = aggreg.factor,
              "planar" = is.planar)
  # format and return results:
  lapply(res, c, use.names = FALSE)
}
