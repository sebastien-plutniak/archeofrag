
#'	@export
frag.get.layers.pair  <- function(graph, layer.attr, sel.layers, size.mini=2, mixed.components.only=FALSE)
{
  if( ! is.igraph(graph))  stop("Not a graph object")
  if(is.null(vertex_attr(graph, layer.attr)))   stop("The parameter 'layer.attr' is required.")
  if( ! is.character(layer.attr))  stop("The parameter 'layer.attr' requires a character value.")
  if( ! layer.attr %in% names(vertex_attr(graph)) ){
    stop(paste("No '", layer.attr, "' vertices attribute", sep=""))
  }
  if(! is.logical(mixed.components.only)) stop("The 'mixed.components.only' parameter requires a logical value.")
  if(! is.numeric(size.mini)) stop("The 'size.mini' parameter requires a numerical value.")
  
  V(graph)$tmp <- vertex_attr(graph, layer.attr)
  
  if(sum(sel.layers %in% V(graph)$tmp) != 2 ){
    stop(paste(c("The values '", sel.layers[1], "' and/or '", sel.layers[2], "' are missing in the '",  layer.attr, "' vertices attribute."),
               sep=" ", collapse = ""))
  }
  
  subgraph <- induced_subgraph(graph, V(graph)[ V(graph)$tmp %in% sel.layers ])
  
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
    warnings("There is no mixed component between these layers.")
    return(NULL)
  }
  # remove vertex attribute and return result:
  delete_vertex_attr(g, "tmp")
}





