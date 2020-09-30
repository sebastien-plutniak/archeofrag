
#'	@export

frag.get.parameters <- function(graph, layer.attr){
  if(!is.igraph(graph)) stop("Not a graph object")
  if(is_directed(graph)) stop("graph must be an undirected igraph object.")
  if(! is.character(layer.attr) ) stop("'layer.attr' invalid")
  if(is.null(vertex_attr(graph, layer.attr))) stop("'layer.attr' invalid")
  V(graph)$layer <- vertex_attr(graph, layer.attr)
  # balance: proportion of non-disturbed pieces in the two layers:
  v1 <- V(graph)[V(graph)$layer == unique(V(graph)$layer)[1]]
  v2 <- V(graph)[V(graph)$layer == unique(V(graph)$layer)[2]]
  subgraph <- subgraph.edges(graph, E(graph)[ ! v1 %--% v2 ])
  balance <- (table(V(subgraph)$layer) / sum(table(V(subgraph)$layer)) )[1]
  # components balance:
  compo.balance <- sapply(decompose(subgraph), function(x) V(x)$layer[1])
  compo.balance <- round(table(compo.balance)[1] / sum(table(compo.balance)), 2) 
  # disturbance: number of pieces which might have move:
  g.list <- frag.get.layers.pair(graph, "layer", unique(V(graph)$layer), mixed.components.only = TRUE)
  disturbance <- 0
  if(! is.null(g.list)){
    g.list <- decompose(g.list)
    g.list <- sapply(g.list, function(x)
      table(factor(V(x)$layer, levels = unique(V(graph)$layer))) )
    g.list <- apply(g.list, 2, function(x){ x[order(x)][2] <- NA ; x })
    disturbance <- sum(g.list, na.rm = TRUE) / gorder(graph)
  } 
  # degree of aggregation of the edges on the components:
  aggreg.factor <- 1 - 1/(1 + sd(sapply(decompose(graph), gsize)))
  
  res <- list(n.components = clusters(graph)$no,
              vertices =  gorder(graph),
              edges = gsize(graph),
              balance = balance,
              components.balance = compo.balance,
              disturbance = disturbance, 
              aggreg.factor = aggreg.factor)
  lapply(res, c, use.names = FALSE)
}
