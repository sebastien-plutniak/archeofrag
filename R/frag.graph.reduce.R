

.reduce.without.conservation <- function(g, final.frag.nr) {
  # delete the exact number of vertices, with no warranty to conserve the number of connected components
  selected.vertice <- sample(igraph::V(g), 1)
  g2 <- igraph::delete_vertices(g, selected.vertice)
  g2 <-
    igraph::delete_vertices(g2, igraph::V(g2)[igraph::degree(g2) == 0])
  if (!igraph::gorder(g2) < final.frag.nr) {
    g <- igraph::delete_vertices(g, selected.vertice)
    g <-
      igraph::delete_vertices(g, igraph::V(g)[igraph::degree(g) == 0])
  }
  g
}

.reduce.with.conservation <- function(g) {
  # delete as much vertices as possible while no decreasing the number of connected components
  igraph::V(g)$component.id <- igraph::clusters(g)$membership
  noDyad <- names(table(igraph::V(g)$component.id)[table(igraph::V(g)$component.id) > 2])
  frag.selection <- (igraph::V(g)$component.id %in% noDyad) & (igraph::degree(g) == 1)
  if (sum(frag.selection) > 0) {
    selected.frag.to.delete <- sample(igraph::V(g)[frag.selection], 1)
    g <- igraph::delete_vertices(g, selected.frag.to.delete)
  }
  g
}


frag.graph.reduce <- function(graph = NULL, n.frag.to.remove = NULL, conserve.objects.nr = FALSE) {
  .check.frag.graph(graph)
  if( ! is.logical(conserve.objects.nr)){stop("'conserve.objects.nr' must be TRUE or FALSE.")}
  if( ! is.numeric(n.frag.to.remove)){stop("'n.frag.to.remove' must be an integer value.")}
  
  final.frag.nr <- igraph::gorder(graph) - n.frag.to.remove
  
  if (conserve.objects.nr) {
    is.reducible <- TRUE
    while ((igraph::gorder(graph) > final.frag.nr) & is.reducible) {
      size.save <- igraph::gorder(graph)
      graph <- .reduce.with.conservation(graph)
      if(igraph::gorder(graph) == size.save){is.reducible <- FALSE}
    }
  } else {
    while (igraph::gorder(graph) > final.frag.nr) {
      graph <- .reduce.without.conservation(graph, final.frag.nr)
    }
  }
  graph
}


