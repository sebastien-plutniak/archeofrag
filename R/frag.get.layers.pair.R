

frag.get.layers.pair  <- function(graph, layer.attr, sel.layers, size.mini=2, mixed.components.only=FALSE, verbose=TRUE)
{
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  if(verbose & ! is.logical(mixed.components.only)) stop("The 'mixed.components.only' parameter requires a logical value.")
  if(verbose & ! is.numeric(size.mini)) stop("The 'size.mini' parameter requires a numerical value.")
  
  igraph::V(graph)$tmp <- igraph::vertex_attr(graph, layer.attr)
  
  if(verbose & (sum(sel.layers %in% igraph::V(graph)$tmp) != 2)){
    stop(paste(c("The values '", sel.layers[1], "' and/or '", sel.layers[2], "' are missing in the '",  layer.attr, "' vertices attribute."),
               sep=" ", collapse = ""))
  }
  # main function:
  subgraph <- igraph::induced_subgraph(graph,
                                       igraph::V(graph)[ igraph::V(graph)$tmp %in% sel.layers ])
  
  igraph::V(subgraph)$membership <- igraph::components(subgraph)$membership
  
  g.list <- igraph::decompose(subgraph)   
  
  if(mixed.components.only == TRUE ){
    sel.components <- sapply(g.list, function(x){
      (length(unique(V(x)$tmp)) != 1)  & (igraph::gorder(x) >= size.mini)
    })
  }else{
    sel.components <- sapply(g.list, function(x) igraph::gorder(x) >= size.mini)
  }
  
  g <- igraph::induced_subgraph(subgraph, 
             igraph::V(subgraph)[igraph::V(subgraph)$membership %in% which(sel.components)])
  if(verbose & igraph::gorder(g) == 0){
    warning("There is no mixed component between these layers.")
    return(NULL)
  }
  # remove vertex attribute and return result:
  igraph::delete_vertex_attr(g, "tmp")
}





