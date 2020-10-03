.set.edge.weight <- function(g){
  # set edges weight = sum of the degrees of their vertices:
  if(gsize(g) == 0 )  return(0)
  e.list <- as_edgelist(g)
  colnames(e.list) <- c("src", "tgt")
  v.list <- cbind(name = V(g)$name, deg = igraph::degree(g))
  e.list <- merge(e.list, v.list, by.x = "src", by.y = "name", sort = F)
  e.list <- merge(e.list, v.list, by.x = "tgt", by.y = "name", sort = F)
  e.list[, 3:4] <- apply(e.list[, 3:4], 2, as.numeric)
  e.list$weight <- apply(e.list[, 3:4], 1, sum)
  E(g)$weight <- e.list$weight
  e.list$weight
}

#'	@export
frag.edges.weighting <- function (graph, layer.attr) 
{
  if (! is.igraph(graph)) stop("Not a graph object")
  if (! is.character(layer.attr)) stop("'layer.attr' invalid")
  if (is.null(layer.attr)) stop("no 'layer.attr' argument")
  
  layers <- vertex_attr(graph, layer.attr)
  if (length(unique(layers)) > 2) 
    stop("There are more than two layers.")
  if (is.null(V(graph)$name)){
    V(graph)$name <- seq(1:gorder(graph))
  }
  layers.u <- unique(layers)
  v1 <- layers == layers.u[1]
  v2 <- layers == layers.u[2]
  
  E(graph)$id <- 1:gsize(graph)
  
  g1 <- subgraph.edges(graph, E(graph)[ V(graph)[v1] %--% V(graph)[v1] ])
  # edges index for layer 1:
  e1 <- E(graph)$id %in% E(g1)$id
  
  # compute edge weights
  E(graph)[ e1 ]$weight <- .set.edge.weight(g1) 
  # add tag to internal edges:
  E(graph)[ e1 ]$scope <- "intra"
  
  if(length(layers.u) == 2){
    g2  <- subgraph.edges(graph, E(graph)[ V(graph)[v2] %--% V(graph)[v2] ])
    g12 <- subgraph.edges(graph, E(graph)[ V(graph)[v1] %--% V(graph)[v2] ] )
    # edges index for layer 2:
    e2 <- E(graph)$id %in% E(g2)$id
    
    # compute layer 2 weights:
    E(graph)[ E(graph)$id %in% E(g2)$id ]$weight <- .set.edge.weight(g2)
    
    # compute ratio between the two layers:
    #~     vertice.ratio <- sort(table(V(graph)$layers))
    #~     vertice.ratio <- vertice.ratio[1] / vertice.ratio[2]
    
    cohesive.ratio <- c(sum(E(graph)[ e1 ]$weight),
                        sum(E(graph)[ e2 ]$weight))
    cohesive.ratio <- sort(cohesive.ratio)
    cohesive.ratio <- cohesive.ratio[1] / cohesive.ratio[2]  # in the unlikely case of a graph with 2 fragments from 2 different layers, this produces NaN
    cohesive.ratio <-  1 + 1 / (1 + cohesive.ratio)
    
    # compute edge weights:
    E(graph)[ E(graph)$id %in% E(g12)$id ]$weight <- .set.edge.weight(g12) * cohesive.ratio
    # add tag to edges:
    E(graph)[ e2 ]$scope <- "intra"
    E(graph)[ E(graph)$id %in% E(g12)$id ]$scope <- "extra"
  }
  return(graph)
}
