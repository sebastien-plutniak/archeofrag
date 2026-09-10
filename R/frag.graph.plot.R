frag.graph.plot  <- function(graph, layer.attr=NULL, node.size=4.5,  ...){
   # tests ----
   .check.frag.graph(graph)
    if(is.null(igraph::graph_attr(graph, "frag_type"))) stop("The 'frag_type' graph attribute is missing")
   
   # if any, rename vertex attribute = x, y, z (to avoid conflict with plot.igraph())
   coords <- c("x", "y", "z")
   coord.names.check <- coords %in% igraph::vertex_attr_names(graph)
   
   rename.vertex.attribute <- function(g, attr){
     g <- igraph::set_vertex_attr(g, toupper(attr), value = igraph::vertex_attr(g, attr))
     igraph::delete_vertex_attr(g, attr)
   }
   
   if(any(coord.names.check)){
     coords <- coords[coord.names.check]
     graph <- Reduce(rename.vertex.attribute, coords, graph)
   }
   
   # check layer attribute:
   # if null, set a default 'layer' attribute:
   if(is.null(layer.attr)){
     graph <- igraph::set_vertex_attr(graph, "layer", value = 1)
     layer.attr <- "layer"
   }
   .check.layer.argument(graph, layer.attr)
   
   # main function:
    igraph::V(graph)$layers <- igraph::vertex_attr(graph, layer.attr)
    
    nLayers <- length(unique(igraph::V(graph)$layers))
    colors <- c("#BBDF27FF",  "darkorchid4", "darkgoldenrod2","chartreuse3", "darkorange3", "brown3",
                "darksalmon", "firebrick2")
    # default edge color:
    igraph::E(graph)$color <- "grey"
    
    layers <- sort(unique(igraph::V(graph)$layers))
    
    if(igraph::graph_attr(graph, "frag_type") == "connection and similarity relations"){
        graph <- igraph::add_layout_(graph, igraph::with_fr(weights = NULL), igraph::component_wise())
        igraph::E(graph)$color <- as.character(factor(igraph::E(graph)$type_relation, labels = c("green", "gray")))
    } else if(igraph::graph_attr(graph, "frag_type") == "similarity relations"){
      graph <- igraph::add_layout_(graph, igraph::with_fr(weights = NULL))
      igraph::E(graph)$color <- "green"
    } else if(length(layers) == 2){ 
      # prepare coordinates if the graph has two layers:
      coords <- data.frame(layer = igraph::V(graph)$layers, miny = 0, maxy = 100) 
      coords[coords$layer == layers[1],]$miny <- 51 
      coords[coords$layer == layers[2],]$maxy <- 49 
      graph$layout <- igraph::layout_with_fr(graph,  niter = 1000, weights =  NULL,
                                     miny = coords$miny, maxy = coords$maxy  )
    }
    
    igraph::plot.igraph(graph, 
         vertex.color = as.character(factor(igraph::V(graph)$layers,
                                            labels = colors[seq_len(nLayers)] )),  
         vertex.label = NA, 
         vertex.size = node.size,
         edge.width = 2,
         edge.color = igraph::E(graph)$color,
         ...)
}
 
