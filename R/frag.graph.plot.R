#'	@export
frag.graph.plot  <- function(graph, layer.attr,  ...){
    if(! is.igraph(graph) )  stop("Not a graph object")
    if(is.null(graph_attr(graph, "frag_type"))) stop("The 'frag_type' graph attribute is missing")
    if(is.null(layer.attr)) stop("'layer.attr' is required")
    if(is.null(vertex_attr(graph, layer.attr))) stop("'layer.attr' invalid")
    V(graph)$layers <- vertex_attr(graph, layer.attr)
    
    nLayers <- length(unique(V(graph)$layers))
    colors <- c("firebrick2","darkorchid4", "chartreuse3", "darkorange3", "brown3",
                "darkgoldenrod2","darkolivegreen3", "darksalmon", rainbow(nLayers))
    
    if(get.graph.attribute(graph, "frag_type") == "connection and similarity relations"){
      graph <- add_layout_(graph, with_fr(), component_wise())
    }    
    
    plot(graph, 
         vertex.color = as.character(factor(V(graph)$layers, labels = colors[1:nLayers] )),  
         vertex.label = NA, 
         vertex.size = 4.5,
         edge.width = 2,
         ...)
    invisible(NULL)
}
 
