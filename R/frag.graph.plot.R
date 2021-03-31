frag.graph.plot  <- function(graph, layer.attr,  ...){
    if(! is.igraph(graph) )  stop("Not a graph object")
    if(is.null(graph_attr(graph, "frag_type"))) stop("The 'frag_type' graph attribute is missing")
    if(is.null(layer.attr)) stop("'layer.attr' is required")
    if( ! is.character(layer.attr))  stop("The parameter 'layer.attr' requires a character value.")
    if(is.null(vertex_attr(graph, layer.attr))){
      stop(paste("No '", layer.attr, "' vertices attribute", sep=""))
    } 
  
    V(graph)$layers <- vertex_attr(graph, layer.attr)
    
    nLayers <- length(unique(V(graph)$layers))
    colors <- c("firebrick2","darkorchid4", "chartreuse3", "darkorange3", "brown3",
                "darkgoldenrod2","darkolivegreen3", "darksalmon", rainbow(nLayers))
    # default edge color:
    E(graph)$color <- "grey"
    
    layers <- sort(unique(V(graph)$layers))
    
    if(graph_attr(graph, "frag_type") == "connection and similarity relations"){
        graph <- add_layout_(graph, with_fr(), component_wise())
        E(graph)$color <- as.character(factor(E(graph)$type_relation, labels = c("green", "gray")))
    } else if(graph_attr(graph, "frag_type") == "similarity relations"){
      graph <- add_layout_(graph, with_fr(), component_wise())
    } else if(length(layers) == 2){ 
      # prepare coordinates if the graph has two layers:
      coords <- data.frame(layer = V(graph)$layers, miny = 0, maxy = 100) 
      coords[coords$layer == layers[1],]$miny <- 51 
      coords[coords$layer == layers[2],]$maxy <- 49 
      graph$layout <- layout_with_fr(graph,  niter= 1000, weights =  NULL,
                                     miny = coords$miny, maxy = coords$maxy  )
    }
    
    plot(graph, 
         vertex.color = as.character(factor(V(graph)$layers, labels = colors[1:nLayers] )),  
         vertex.label = NA, 
         vertex.size = 4.5,
         edge.width = 2,
         edge.color = E(graph)$color,
         ...)
    invisible(NULL)
}
 
