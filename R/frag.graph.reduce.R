

.reduce.without.conservation <- function(g, final.frag.nr) {
  # delete the exact number of vertices, with no insurance to preserve the number of connected components
  selected.vertice <- sample(igraph::V(g), 1)
  g2 <- igraph::delete_vertices(g, selected.vertice)
  g2 <- igraph::delete_vertices(g2, igraph::V(g2)[igraph::degree(g2) == 0]) # delete singletons
  if (igraph::gorder(g2) >= final.frag.nr) {
    g <- igraph::delete_vertices(g, selected.vertice)
    g <- igraph::delete_vertices(g, igraph::V(g)[igraph::degree(g) == 0])
  } else{ g$is.reducible <- FALSE }
  g
}

.reduce.with.conservation <- function(g) {
  # delete a vertice as far as it does not decrease the number of connected components
  igraph::V(g)$component.id <- igraph::components(g)$membership
  noDyad <- names(table(igraph::V(g)$component.id)[table(igraph::V(g)$component.id) > 2])
  # exclude vertices in dyadic components and articulations points:
  frag.selection <- (igraph::V(g)$component.id %in% noDyad) & ( ! V(g) %in% igraph::articulation_points(g) )
  
  if (sum(frag.selection)) {
    selected.frag.to.delete <- sample(igraph::V(g)[frag.selection], 1)
    g <- igraph::delete_vertices(g, selected.frag.to.delete)
  } else{ g$is.reducible <- FALSE }
  g
}


frag.graph.reduce <- function(graph = NULL, n.frag.to.remove = NULL, conserve.objects.nr = FALSE) {
  .check.frag.graph(graph)
  if( ! is.logical(conserve.objects.nr)){stop("'conserve.objects.nr' must be TRUE or FALSE.")}
  if( ! is.numeric(n.frag.to.remove)){stop("'n.frag.to.remove' must be an integer value.")}
  
  graph$is.reducible <- TRUE
  
  final.frag.nr <- igraph::gorder(graph) - n.frag.to.remove
  
  if (conserve.objects.nr) {
    is.reducible <- TRUE
    while ((igraph::gorder(graph) > final.frag.nr) & graph$is.reducible) {
      graph <- .reduce.with.conservation(graph)
    }
  } else {
    while ((igraph::gorder(graph) > final.frag.nr) & graph$is.reducible) {
      graph <- .reduce.without.conservation(graph, final.frag.nr)
    }
  }
  igraph::delete_graph_attr(graph, "is.reducible")
}



 