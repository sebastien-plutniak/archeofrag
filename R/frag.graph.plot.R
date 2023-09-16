frag.graph.plot  <- function(graph, layer.attr,  ...){
   # tests:
   .check.frag.graph(graph)
   .check.layer.argument(graph, layer.attr)
    if(is.null(igraph::graph_attr(graph, "frag_type"))) stop("The 'frag_type' graph attribute is missing")
   
   # main function:
    igraph::V(graph)$layers <- igraph::vertex_attr(graph, layer.attr)
    
    nLayers <- length(unique(igraph::V(graph)$layers))
    colors <- c("firebrick2", "darkorchid4", "chartreuse3", "darkorange3", "brown3",
                "darkgoldenrod2","darkolivegreen3", "darksalmon")
    # default edge color:
    igraph::E(graph)$color <- "grey"
    
    layers <- sort(unique(igraph::V(graph)$layers))
    
    if(igraph::graph_attr(graph, "frag_type") == "connection and similarity relations"){
        graph <- igraph::add_layout_(graph, igraph::with_fr(), igraph::component_wise())
        igraph::E(graph)$color <- as.character(factor(igraph::E(graph)$type_relation, labels = c("green", "gray")))
    } else if(igraph::graph_attr(graph, "frag_type") == "similarity relations"){
      graph <- igraph::add_layout_(graph, igraph::with_fr())
      igraph::E(graph)$color <- "green"
    } else if(length(layers) == 2){ 
      # prepare coordinates if the graph has two layers:
      coords <- data.frame(layer = igraph::V(graph)$layers, miny = 0, maxy = 100) 
      coords[coords$layer == layers[1],]$miny <- 51 
      coords[coords$layer == layers[2],]$maxy <- 49 
      graph$layout <- igraph::layout_with_fr(graph,  niter= 1000, weights =  NULL,
                                     miny = coords$miny, maxy = coords$maxy  )
    }
    
    igraph::plot.igraph(graph, 
         vertex.color = as.character(factor(igraph::V(graph)$layers,
                                            labels = colors[1:nLayers] )),  
         vertex.label = NA, 
         vertex.size = 4.5,
         edge.width = 2,
         edge.color = igraph::E(graph)$color,
         ...)
}
 
