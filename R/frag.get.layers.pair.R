
#'	@export
frag.get.layers.pair  <- function(graph, layer.attr, sel.layers, size.mini=2, mixed.components.only=FALSE)
{
  if(! is.igraph(graph))  stop("Not a graph object")
  if(is.null(vertex_attr(graph, layer.attr)))   stop("'layer.attr' invalid")
  if(! is.character(layer.attr))  stop("'layer.attr' invalid")
  V(graph)$tmp <- vertex_attr(graph, layer.attr)
  
  if(sum(sel.layers %in% V(graph)$tmp) != 2 ) stop("The two 'selected layers' are not in the 'layers' vector")
  
  subgraph <- induced_subgraph(graph, V(graph)[ V(graph)$tmp %in% sel.layers ])
  
  if(gorder(subgraph)) return(subgraph)
  
  V(subgraph)$membership <- clusters(subgraph)$membership
  
  g.list <- decompose(subgraph)   
  
  if(mixed.components.only == TRUE ){
    sel.components <- sapply(g.list, function(x){
      (length(unique(V(x)$tmp)) != 1)  & (gorder(x) >= size.mini)
    })
  }else{
    sel.components <- sapply(g.list, function(x) gorder(x) >= size.mini)
  }
  
  g <- induced_subgraph(subgraph, V(subgraph)[V(subgraph)$membership %in% which(sel.components)])
  if(gorder(g) == 0){
    warnings("No mixed component between these layers")
    return(NULL)
  }
  delete_vertex_attr(g, "tmp")
}





