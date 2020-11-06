.set.edge.weight <- function(g){
  #edges weight = sum of the degrees * transitivity-based index
  if(gsize(g) == 0 )  return(0)
  e.list <- as_edgelist(g)
  colnames(e.list) <- c("src", "tgt")
  v.list <- cbind(name = V(g)$name,
                  deg = igraph::degree(g),
                  trans = transitivity(g, type = "localundirected",
                                          isolates = "zero", weights = NULL))
  e.list <- merge(e.list, v.list, by.x = "src", by.y = "name", sort = F)
  e.list <- merge(e.list, v.list, by.x = "tgt", by.y = "name", sort = F)
  e.list[, 3:6] <- apply(e.list[, 3:6], 2, as.numeric)
  
  e.list$SumDegree <- e.list$deg.x + e.list$deg.y
  e.list$trans.factor <- apply(e.list[, c(4,6)], 1, function(trans){
     3 - (2 / (1 + mean(trans)) )  
  })
  size.factor <- (1 - (1 / ( sqrt(gorder(g) + gsize(g)) ) ))^2
  e.list$SumDegree * e.list$trans.factor * size.factor
}

frag.edges.weighting <- function (graph, layer.attr) 
{
  if (! is.igraph(graph)) stop("Not a graph object")
  if (! is.character(layer.attr)) stop("'layer.attr' invalid")
  if (is.null(layer.attr)) stop("no 'layer.attr' argument")
  
  layers <- vertex_attr(graph, layer.attr)
  layers.u <- unique(layers)
  
  if (length(layers.u) > 2) 
    stop("There are more than two layers.")
  if (is.null(V(graph)$name)){
    V(graph)$name <- seq(1:gorder(graph))
  }
  v1 <- layers == layers.u[1]
  v2 <- layers == layers.u[2]
  E(graph)$id <- 1:gsize(graph)
  
  g1  <- subgraph.edges(graph, E(graph)[ V(graph)[v1] %--% V(graph)[v1] ])
  g2  <- subgraph.edges(graph, E(graph)[ V(graph)[v2] %--% V(graph)[v2] ])
  g12 <- subgraph.edges(graph, E(graph)[ V(graph)[v1] %--% V(graph)[v2] ])
  
  # edges indices:
  e1  <- E(graph)$id %in% E(g1)$id
  e2  <- E(graph)$id %in% E(g2)$id
  e12 <- E(graph)$id %in% E(g12)$id
  # compute weights: 
  E(graph)[ e1 ]$weight <- .set.edge.weight(g1)
  E(graph)[ e2 ]$weight <- .set.edge.weight(g2)
  E(graph)[ e12]$weight <- .set.edge.weight(g12)
  # add tag to edges:
  E(graph)$scope <- "intra"
  E(graph)[e12]$scope <- "extra"
  graph
}
