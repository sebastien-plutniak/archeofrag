frag.relations.by.layers <- function(graph, layer.attr){
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # retrieve the layer argument:
  layers <- igraph::vertex_attr(graph, layer.attr)
  
  if(is.null(igraph::V(graph)$name)){
    igraph::V(graph)$name <- 1:igraph::gorder(graph)
  }
  
  v.list <- data.frame("v" = igraph::V(graph)$name, "layer" = layers)
  e.list <- data.frame(igraph::as_edgelist(graph))
  e.list <- merge(e.list, v.list, by.x = "X2", by.y = "v")
  e.list <- merge(e.list, v.list, by.x = "X1", by.y = "v")
  
  res <- table(e.list$layer.x, e.list$layer.y)
  diag <- diag(res)
  res <- res + matrix(res, nrow(res), byrow=TRUE)
  diag(res) <- diag
  res[upper.tri(res)] <- NA
  res
}
