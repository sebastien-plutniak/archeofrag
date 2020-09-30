
#'	@export
frag.relations.by.layers <- function(graph, layer.attr){
  if(! is.igraph(graph)) stop("Not an igraph object")
  if(! is.character(layer.attr))  stop("'layer.attr' invalid")
  if(is.null(vertex_attr(graph, layer.attr))) stop("'layer.attr' invalid")
  layers <- vertex_attr(graph, layer.attr)
  
  if(is.null(V(graph)$name)){
    V(graph)$name <- 1:gorder(graph)
  }
  
  v.list <- data.frame(v = V(graph)$name, layer = layers)
  e.list <- data.frame(as_edgelist(graph))
  e.list <- merge(e.list, v.list, by.x = "X2", by.y = "v")
  e.list <- merge(e.list, v.list, by.x = "X1", by.y = "v")
  
  res <- table(e.list$layer.x, e.list$layer.y)
  res[lower.tri(res)] <- res[upper.tri(res)] + res[lower.tri(res)]
  res[upper.tri(res)] <- res[lower.tri(res)]
  res
}
