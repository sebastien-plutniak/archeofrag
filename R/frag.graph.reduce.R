
.remove.vertice.not.conserving.components.nr <- function(g, final.frag.nr) {
  # exclude protected vertices if any:
  vertice.selection <- ! igraph::V(g)$preserve  
  
  if ( ! sum(vertice.selection)) {
    g$is.reducible <- FALSE 
    return(g)
  }
  vertice.selection <- sample(which(vertice.selection), 1)
  selected.vertice.to.delete <- igraph::V(g)[vertice.selection]
    
  # delete on a copy of the graph:
  g2 <- igraph::delete_vertices(g, selected.vertice.to.delete)
  g2 <- igraph::delete_vertices(g2, igraph::V(g2)[igraph::degree(g2) == 0 & ! igraph::V(g2)$preserve]) # delete singletons

  # delete on the actual graph:
  if (igraph::gorder(g2) >= final.frag.nr) {
    g <- igraph::delete_vertices(g, selected.vertice.to.delete)
    g <- igraph::delete_vertices(g, igraph::V(g)[igraph::degree(g) == 0 & ! igraph::V(g)$preserve])
  } else{ igraph::V(g)[vertice.selection]$preserve <- TRUE } # exclude the selected vertice for the next loop
  g
}

.remove.vertice.conserving.components.nr <- function(g) {
  # select and delete a vertice if it does not decrease the number of connected components
  # (exclude vertices in dyadic components, articulations points, and with edges between spatial units:
  igraph::V(g)$component.id <- igraph::components(g)$membership
  not.dyadic <- names(table(igraph::V(g)$component.id)[table(igraph::V(g)$component.id) > 2])
  
  vertice.selection <- (igraph::V(g)$component.id %in% not.dyadic) &
    ( ! igraph::V(g) %in% igraph::articulation_points(g) ) & 
    ( ! igraph::V(g)$preserve )
  
  if (sum(vertice.selection)) {
    selected.vertice.to.delete <- sample(igraph::V(g)[vertice.selection], 1)
    g <- igraph::delete_vertices(g, selected.vertice.to.delete)
  } else{ g$is.reducible <- FALSE }
  g
}


.reduce.graph <- function(graph, final.frag.nr, conserve.objects.nr){
  graph$is.reducible <- TRUE
  
  if(conserve.objects.nr){
    while ((igraph::gorder(graph) > final.frag.nr) & graph$is.reducible) {
      graph <- .remove.vertice.conserving.components.nr(graph)
    }
  } else {
    while ((igraph::gorder(graph) > final.frag.nr) & graph$is.reducible) {
      graph <- .remove.vertice.not.conserving.components.nr(graph, final.frag.nr)
    }
  }
  igraph::delete_graph_attr(graph, "is.reducible")
}


.concatenate.attr <- function(graph, attribute){
  attr <- c(igraph::vertex_attr(graph, paste0(attribute, "_1")), 
            igraph::vertex_attr(graph, paste0(attribute, "_2")) 
  )
  attr[ ! is.na(attr)]
}


frag.graph.reduce <- function(graph = NULL, n.frag.to.remove = NULL, conserve.objects.nr = FALSE, conserve.frag.balance = FALSE, conserve.inter.units.connection = FALSE, verbose = FALSE) {
  .check.frag.graph(graph)
  if( ! is.logical(conserve.objects.nr)){stop("'conserve.objects.nr' must be TRUE or FALSE.")}
  if( ! is.numeric(n.frag.to.remove)){stop("'n.frag.to.remove' must be an integer value.")}
  
  igraph::V(graph)$preserve <- F 
  
  v1 <- igraph::V(graph)$layer == sort(unique(igraph::V(graph)$layer))[1] 
  v2 <- igraph::V(graph)$layer == sort(unique(igraph::V(graph)$layer))[2] 
  inter.units.edges <- igraph::E(graph)[ v1 %--% v2 ]
  
  # tag vertices with inter-unit edges to keep:
  if(conserve.inter.units.connection){
    v.to.preserve <- unique(c(igraph::as_edgelist(graph)[inter.units.edges,]))
    igraph::V(graph)[ igraph::V(graph)$name %in% v.to.preserve ]$preserve <- T
  }
  
  if(conserve.frag.balance){
    frag.balance.unit.1 <- frag.get.parameters(graph, "layer")$balance
    frag.balance.unit.2 <- 1 - frag.balance.unit.1
    n.frag.to.remove.unit.1 <- floor(n.frag.to.remove * frag.balance.unit.1)
    n.frag.to.remove.unit.2 <- floor(n.frag.to.remove * frag.balance.unit.2)
    
    # subgraphs
    unit.1.g <- igraph::induced_subgraph(graph, igraph::V(graph)[ v1 ] )
    unit.2.g <- igraph::induced_subgraph(graph, igraph::V(graph)[ v2 ] )
    
    final.frag.nr.unit.1 <- igraph::gorder(unit.1.g) - n.frag.to.remove.unit.1
    final.frag.nr.unit.2 <- igraph::gorder(unit.2.g) - n.frag.to.remove.unit.2

    if(final.frag.nr.unit.1 < 2) {final.frag.nr.unit.1 <- 2}
    if(final.frag.nr.unit.2 < 2) {final.frag.nr.unit.2 <- 2}
    
    # reduce subgraphs:
    unit.1.g <- .reduce.graph(unit.1.g, final.frag.nr = final.frag.nr.unit.1, conserve.objects.nr = conserve.objects.nr)
    unit.2.g <- .reduce.graph(unit.2.g, final.frag.nr = final.frag.nr.unit.2, conserve.objects.nr = conserve.objects.nr)
    
    # recombine subgraphs:
    igraph::V(unit.1.g)$name <- as.character(igraph::V(unit.1.g)$name)
    igraph::V(unit.2.g)$name <- as.character(igraph::V(unit.2.g)$name)
    graph.recombined <- igraph::union(unit.1.g, unit.2.g, byname = TRUE)
    
    # add inter-units edges:
    e.list <- igraph::as_edgelist(graph)[inter.units.edges, ]
    e.list <- e.list[e.list[, 1] %in% igraph::V(graph.recombined)$name & e.list[, 2] %in% igraph::V(graph.recombined)$name, ]
    e.list <- matrix(e.list, ncol = 2)
    
    if(nrow(e.list) > 0){
      graph <- igraph::add_edges(graph.recombined, as.character(c(rbind(e.list[, 1], e.list[, 2]))))
    } else {
      graph <- graph.recombined
    }
    
    # reformat attributes:
    igraph::V(graph)$layer <- .concatenate.attr(graph, "layer")
    igraph::V(graph)$morphometry <- .concatenate.attr(graph, "morphometry")
    
    graph <- Reduce(igraph::delete_vertex_attr, c("morphometry_1", "morphometry_2",
                                                   "object.id_1", "object.id_2",
                                                   "layer_1", "layer_2",
                                                  "preserve_1", "preserve_2"), 
                     graph)
    
    graph <- Reduce(igraph::delete_edge_attr, c("weight_1", "weight_2",
                                                 "scope_1", "scope_2",
                                                 "id_1", "id_2"), 
                     graph)
    graph$frag_type <- graph$frag_type_1
    graph <- Reduce(igraph::delete_graph_attr, c("frag_type_1", "frag_type_2"), graph)
      
  } else {
    final.frag.nr <- igraph::gorder(graph) - n.frag.to.remove
    graph.reduced <- .reduce.graph(graph, final.frag.nr, conserve.objects.nr)
    
    # add inter-units edges:
    e.list <- igraph::as_edgelist(graph)[inter.units.edges, ]
    e.list <- e.list[e.list[, 1] %in% igraph::V(graph.reduced)$name & e.list[, 2] %in% igraph::V(graph.reduced)$name, ]
    e.list <- matrix(e.list, ncol = 2)
    
    if(nrow(e.list) > 0){
      graph <- igraph::add_edges(graph.reduced, as.character(c(rbind(e.list[, 1], e.list[, 2]))))
    } else {
      graph <- graph.reduced
    }
    
  }
  graph <- igraph::simplify(graph)
  igraph::E(graph)$weight <- 1 # reset weight values
  igraph::delete_vertices(graph, igraph::V(graph)[igraph::degree(graph) == 0])
}


