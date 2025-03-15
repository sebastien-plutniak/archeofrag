
.run.simul <- function(iter, observed.graph, layer.attr, initial.layer, verbose, ...){
  res <- lapply(seq_len(iter), function(i){
    g <- frag.simul.process(initial.layers = initial.layer,
                            from.observed.graph = observed.graph,
                            observed.layer.attr = layer.attr,  ...)
    # measure the properties of the graph:
    params <- frag.get.parameters(g, "layer", verbose = verbose)
    c(
      "edges" = params$edges,
      "balance" = params$balance,
      "components.balance" = params$components.balance,
      "disturbance" = params$disturbance,
      frag.layers.admixture(g, "layer", verbose = verbose),
      "cohesion" = rbind(frag.layers.cohesion(g, "layer", verbose = verbose)),
      "edge.weights.sum" = sum(igraph::E(g)$weight),
      "edge.weights.median" = params$edge.weights.median,
      "edge.weights.median.abs.dev." = params$edge.weights.median.abs.dev.
    )
  })
  res <- do.call(rbind, res)
  data.frame(res)
}  

frag.simul.compare <- function(graph, layer.attr, iter, summarise=TRUE, verbose=TRUE, ...){
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  if(iter < 30) stop("At least 30 iterations are required.")
  if(! is.logical(summarise)){
    stop("A logical value is required for the 'summary' parameter.")
  }
  # main function:
  resH1 <- .run.simul(iter, graph, layer.attr, initial.layer = 1, verbose = verbose, ...)
  resH2 <- .run.simul(iter, graph, layer.attr, initial.layer = 2, verbose = verbose, ...)
  
  if(! summarise){
    return(list("h1.data" = resH1, "h2.data" = resH2))
  }
  stats <- frag.simul.summarise(graph, layer.attr, resH1, resH2, verbose = verbose)
  # print the summary table:
  print(stats)
  #return silently the results as a list:
  invisible(list("h1.data" = resH1, "h2.data" = resH2, "summary" = stats))
}
