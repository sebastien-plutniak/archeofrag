

.detect.cycle <- function(graph, x){
  pattern <- igraph::make_ring(x) # create the pattern to detect:
  cycles.list <- igraph::subgraph_isomorphisms(pattern, graph)
  res <- data.frame()
  if(length(cycles.list) != 0){ 
    res <- do.call(rbind, cycles.list) # convert the list to a data frame
    res <- res[ ! duplicated(t(apply(res, 1, sort))), ]
  }
  res
}

.max.cycles.only <- function(x, res){
  if(x-1 == 0) return(res) # control
  remove.id <- sapply(split(res[[x]], row(res[[x]])), 
                      function(i) sum(i %in% res[[x-1]]) == length(i) )
  res[[x]] <- res[[x]][ ! remove.id, ]
  .max.cycles.only(x-1, res) # recursive call
}

# 
frag.cycles <- function(graph, kmax, max.cycles.only=FALSE) {
  # tests:
  .check.frag.graph(graph)  
  
  if(igraph::graph_attr(graph, "frag_type") == "connection and similarity" ){
    warning("Cycle detection in a 'connection and similarity' fragmentation graph is meaningless.")
  }
  if( ! is.numeric(kmax)){
    stop("The 'k' parameter must be numerical.")
  } else  if(kmax < 3) {
    stop("k must be >= 3")
  }
  if( ! is.logical(max.cycles.only)) stop("The 'max.cycles.only' parameter must be logical.")
  
  results <- lapply(kmax:3, function(x) .detect.cycle(graph, x))
  if(max.cycles.only){
    results <- .max.cycles.only(length(results), results)
  }
  results <- rev(results)
  results <- sapply(1:length(results), function(x) length(c(results[[x]])) / (x + 2) ) 
  names(results) <- sapply(3:kmax, function(x) paste(x, "-cycles", sep=""))
  results
}

 
