
.run.simul <- function(iter, observed.graph, layer.attr, initial.layer){
  res <- lapply(1:iter, function(i){
    g <- frag.simul.process(initial.layers = initial.layer,
                            from.observed.graph = observed.graph,
                            observed.layer.attr = layer.attr)
    g <- frag.edges.weighting(g, layer.attr)
    # measure the properties of the graph:
    inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]
    c(
      "edges" = gsize(g),
      "weightsum" = sum(E(g)$weight),
      "balance" = c(sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)), use.names=FALSE),
      "disturbance" = length(inter.layer.e) / gsize(g),
      frag.layers.admixture(g, "layer"),
      "cohesion" = rbind(frag.layers.cohesion(g, "layer"))
    )
  })
  res <- do.call(rbind, res)
  data.frame(res)
}  

frag.simul.compare <- function(graph, layer.attr, iter, summarise=TRUE){
  if(! is.igraph(graph)) stop("Not a graph object")
  if(is.null(vertex_attr(graph, layer.attr)))   stop("The parameter 'layer.attr' is required.")
  if( ! is.character(layer.attr))  stop("The parameter 'layer.attr' requires a character value.")
  if( ! layer.attr %in% names(vertex_attr(graph)) ){
    stop(paste("No '", layer.attr, "' vertices attribute.", sep=""))
  }
  if(iter < 30) stop("At least 30 iterations are required.")
  if(! is.logical(summarise)){
    stop("A logical value is required for the 'summary' parameter.")
  }
  
  resH1 <- .run.simul(iter, graph, layer.attr, 1)
  resH2 <- .run.simul(iter, graph, layer.attr, 2)
  
  if(! summarise){
    return(list(h1.data = resH1, h2.data = resH2))
  }
  stats <- frag.simul.summarise(graph, layer.attr, resH1, resH2)
  # print the summary table:
  print(stats)
  #return silently the results as a list:
  invisible(list(h1.data = resH1, h2.data = resH2, summary = stats))
}
