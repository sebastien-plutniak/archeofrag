

.check.frag.graph <- function(graph){
  if(! igraph::is_igraph(graph)) stop("Not a graph object")
  if(igraph::is_directed(graph)) stop("The 'graph' parameter requires an undirected igraph object.")
  if(! igraph::is_simple(graph)) stop("Loops and multiple edges are not allowed.")
}

.check.layer.argument <- function(graph, layer.attr){
  if(is.null(igraph::vertex_attr(graph, layer.attr)))   stop("'layer.attr' is missing or does not correspond to a vertex attribute of the graph.")
  if(is.null(igraph::vertex_attr(graph, layer.attr)))   stop("The parameter 'layer.attr' is required.")
  if (is.null(layer.attr))  stop("No 'layer.attr' argument")
  if( ! is.character(layer.attr))  stop("The parameter 'layer.attr' requires a character value.")
  if( ! layer.attr %in% names(igraph::vertex_attr(graph)) ){
    stop(paste("There is no '", layer.attr, "' vertices attribute.", sep=""))
  }
}

# considering the subgraph including only fragments connected to fragments from the same spatial unit, proportion of fragments in the 1st spatial unit (alphanumerically)
.fragments.balance <- function(g){
  v1 <- igraph::V(g)[igraph::V(g)$layer == unique(igraph::V(g)$layer)[1]]
  v2 <- igraph::V(g)[igraph::V(g)$layer == unique(igraph::V(g)$layer)[2]]
  subgraph <- igraph::subgraph_from_edges(g, igraph::E(g)[ ! v1 %--% v2 ])
  frag.count <- table(igraph::V(subgraph)$layer)
  round(frag.count[1] / sum(frag.count), 2)
}


# considering the subgraph including only fragments connected to fragments from the same spatial unit, proportion of components in the 1st spatial unit (alphanumerically)
.components.balance <- function(g){
  v1 <- igraph::V(g)[igraph::V(g)$layer == unique(igraph::V(g)$layer)[1]]
  v2 <- igraph::V(g)[igraph::V(g)$layer == unique(igraph::V(g)$layer)[2]]
  subgraph <- igraph::subgraph_from_edges(g, igraph::E(g)[ ! v1 %--% v2 ])
  
  compo.balance <- sapply(igraph::decompose(subgraph), 
                          function(x) igraph::V(x)$layer[1])
  compo.balance <- table(compo.balance)
  round(compo.balance[1] / sum(compo.balance), 2) 
}
