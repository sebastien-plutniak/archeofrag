.euclidean.distance <- function(graph, x, y, z){
  coords <- cbind(name = igraph::V(graph)$name,
                  x = igraph::vertex_attr(graph, x),
                  y = igraph::vertex_attr(graph, y),
                  z = igraph::vertex_attr(graph, z))
  e.list <- igraph::as_edgelist(graph)
  colnames(e.list) <- c("name1", "name2")
  e.list <- merge(e.list, coords, by.x="name1", by.y="name", sort=FALSE)
  e.list <- merge(e.list, coords, by.x="name2", by.y="name",
                  suffixes=c("1", "2"), sort=FALSE)
  e.list <- e.list[, order(colnames(e.list))]
  e.list <- e.list[, -c(1, 2)] 
  e.list <- apply(e.list, 2, as.numeric)
  e.list <- matrix(e.list, ncol = (ncol(coords)-1)*2, byrow=T)
  
  if(sum(is.na(e.list)) > 0){
    stop("Cannot compute distances, missing coordinates values.")
  }
  
  if( ncol(e.list) == 4){
    sqrt((e.list[,1] - e.list[,2])^2 + (e.list[,3] - e.list[,4])^2 )
  } else if(ncol(e.list) == 6){
    sqrt((e.list[,1] - e.list[,2])^2 + (e.list[,3] - e.list[,4])^2 + (e.list[,5] - e.list[,6])^2 )
  }
}

.get.morpho.spatial.params <- function(graph, x, y, z){
  # input: a graph and the names for x y z attributes
  # output: a data frame with the distance and morpho parameters for the graph edges
  e.list <- igraph::as_edgelist(graph)
  e.list <- data.frame(e.list) 
  e.list$id <- 1:nrow(e.list) # a an it to sort the row later
  colnames(e.list) <- c("name1", "name2", "id")
  morpho.df <- cbind(name = igraph::V(graph)$name, 
                     morphometry = igraph::V(graph)$morphometry)
  e.list <- merge(e.list, morpho.df, by.x="name1", by.y="name", sort=FALSE)
  e.list <- merge(e.list, morpho.df, by.x="name2", by.y="name",
                  suffixes=c("1", "2"), sort=FALSE)
  e.list <- e.list[order(e.list$id), ] # sort the rows
  e.list <- e.list[, 4:5]
  e.list <- apply(e.list, 2, as.numeric)
  e.list <- data.frame(matrix(e.list, ncol=2))
  colnames(e.list) <- c("morphometry1", "morphometry2")
  
  e.list$proportion <- apply(e.list, 1, function(x){
    v <- sort(x[1:2])
    v[1] / v[2]
  })
  
  # compute spatial distances (this is done in this function to avoid repeating 3 times the command)
  e.list$distance <- 1 # default value
  coords <- c(x, y, z)[ c(x, y, z) %in% igraph::vertex_attr_names(graph) ]
  if( length(coords) > 1) {   # if at least two coordinates
    e.list$distance <- .euclidean.distance(graph, x, y, z) + 1 # +1 to ensure a neutral value when all distances=0
  }
  e.list
}

.morpho.spatial.factor <- function(morpho1, morpho2, proportion, distance,
                                   morpho.max, proportion.max,
                                   distance.max){
  morpho.factor <- (morpho1 + morpho2) / (morpho.max) # +1)
  morpho.prop.factor <- proportion / proportion.max
  distance.factor <- distance / (distance.max) #  +1)
  1 - sqrt(morpho.factor  * sqrt(morpho.prop.factor) * distance.factor)
}

.set.edge.weight <- function(g){
  #edges weight = sum of the degrees * transitivity-based index
  if(igraph::gsize(g) == 0 )  return(0)
  e.list <- igraph::as_edgelist(g)
  colnames(e.list) <- c("src", "tgt")
  v.list <- cbind(name = V(g)$name,
                  deg = igraph::degree(g),
                  trans = igraph::transitivity(g, type = "localundirected",
                                       isolates = "zero", weights = NULL))
  e.list <- merge(e.list, v.list, by.x="src", by.y="name", sort=FALSE)
  e.list <- merge(e.list, v.list, by.x="tgt", by.y="name", sort=FALSE)
  e.list[, 3:6] <- apply(e.list[, 3:6], 2, as.numeric)
  
  e.list$SumDegree <- e.list$deg.x + e.list$deg.y
  e.list$trans.factor <- apply(e.list[, c(4,6)], 1, function(trans){
    3 - (2 / (1 + mean(trans)) )  
  })
  size.factor <- (1 - (1 / ( sqrt(gorder(g) + gsize(g)) ) ))^2
  e.list$SumDegree * e.list$trans.factor * size.factor
}

frag.edges.weighting <- function(graph, layer.attr, morphometry="", x="", y="", z=""){
  # tests:
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  if(! is.character(morphometry)) {
    stop("A character value is expected for the 'morphometry' parameter.")
  }
  if(sum(! sapply(c(x,y,z), is.character)) != 0) {
    stop("Character values are expected for the 'x', 'y' and 'z' parameters.")
  }
  
  if(is.null(V(graph)$name)){
    igraph::V(graph)$name <- seq(1:gorder(graph))
  }
  missing.coords <- c(x, y, z)[ ! c(x, y, z) %in% c("", igraph::vertex_attr_names(graph)) ]
  if(length(missing.coords) > 0){
    warning(paste("Missing coordinates:", paste(missing.coords, collapse = ", ")  ))
  }
  
  igraph::V(graph)$morphometry <- 1 # set default value
  if( morphometry != ""){
    if( morphometry %in% names(igraph::vertex_attr(graph)) ){
      igraph::V(graph)$morphometry <- igraph::vertex_attr(graph, morphometry)
    }else{
      stop(paste("No '", morphometry, "' vertices attribute", sep=""))
    }
  }
  if(! is.numeric(igraph::V(graph)$morphometry)){
    stop("Numeric values are required for the 'morphometry' parameter.")
  }
  
  layers <- igraph::vertex_attr(graph, layer.attr)
  layers.u <- unique(layers)
  if(length(layers.u) > 2){
    stop("There are more than two layers.")
  }
  
  v1 <- layers == layers.u[1]
  v2 <- layers == layers.u[2]
  igraph::E(graph)$id <- 1:igraph::gsize(graph)
  
  # get the max morphometric/spatial values observed in the data set:
  params  <- .get.morpho.spatial.params(graph, x, y, z)
  
  morph.max <- max(params$morphometry1 + params$morphometry2)
  prop.max <- max(params$proportion)
  dist.max <- max(params$distance)
  
  # add a "distance" edge attribute if necessary:
  if(! is.null(params$distance)){
    igraph::E(graph)$distance <- params$distance
  }
  
  # generate subgraphs
  g1  <- igraph::subgraph.edges(graph, igraph::E(graph)[ igraph::V(graph)[v1] %--% igraph::V(graph)[v1] ])
  g2  <- igraph::subgraph.edges(graph, igraph::E(graph)[ igraph::V(graph)[v2] %--% igraph::V(graph)[v2] ])
  g12 <- igraph::subgraph.edges(graph, igraph::E(graph)[ igraph::V(graph)[v1] %--% igraph::V(graph)[v2] ])
  
  # extract edges indices:
  e1  <- igraph::E(graph)$id %in% igraph::E(g1)$id
  e2  <- igraph::E(graph)$id %in% igraph::E(g2)$id
  e12 <- igraph::E(graph)$id %in% igraph::E(g12)$id
  
  # get the morphometric/spatial parameters of the edges compute the 
  # morphometric/spatial factor for the edges  of each subgraphs:
  morpho.spatial.factor.g1 <- 0
  if(igraph::gsize(g1) > 0){
    params1  <- .get.morpho.spatial.params(g1, x, y, z)
    morpho.spatial.factor.g1 <- apply(params1, 1, function(x){
      .morpho.spatial.factor(morpho1 = x[1],
                             morpho2 = x[2],
                             proportion = x[3],
                             distance = x[4],
                             morpho.max = morph.max,
                             proportion.max = prop.max,
                             distance.max = dist.max)
    })
  }
  
  morpho.spatial.factor.g2 <- 0
  if(igraph::gsize(g2) > 0){
    params2  <- .get.morpho.spatial.params(g2, x, y, z)
    morpho.spatial.factor.g2 <- apply(params2, 1, function(x){
      .morpho.spatial.factor(morpho1 = x[1],
                             morpho2 = x[2],
                             proportion = x[3],
                             distance = x[4],
                             morpho.max = morph.max,
                             proportion.max = prop.max,
                             distance.max = dist.max)
    })
  }
  
  morpho.spatial.factor.g12 <- 0
  if(igraph::gsize(g12) > 0){
    params12 <- .get.morpho.spatial.params(g12, x, y, z)
    morpho.spatial.factor.g12 <- apply(params12, 1, function(x){
      .morpho.spatial.factor(morpho1 = x[1],
                             morpho2 = x[2],
                             proportion = x[3],
                             distance = x[4],
                             morpho.max = morph.max,
                             proportion.max = prop.max,
                             distance.max = dist.max)
    })
  }
  
  # compute the weights for the edges of the 3 subsets:
  igraph::E(graph)[ e1 ]$weight <- .set.edge.weight(g1)
  igraph::E(graph)[ e2 ]$weight <- .set.edge.weight(g2)
  igraph::E(graph)[ e12]$weight <- .set.edge.weight(g12)
  #   apply the morpho/spatial modifier is required:
  if(sum(sapply(list(morphometry, x, y, z), function(x) x != ""))){
    igraph::E(graph)[ e1 ]$weight <- igraph::E(graph)[e1]$weight * morpho.spatial.factor.g1
    igraph::E(graph)[ e2 ]$weight <- igraph::E(graph)[e2]$weight * morpho.spatial.factor.g2
    igraph::E(graph)[ e12]$weight <- igraph::E(graph)[e12]$weight* morpho.spatial.factor.g12
  }  
  
  # add tags to the edges:
  igraph::E(graph)$scope <- "intra"
  igraph::E(graph)[e12]$scope <- "extra"
  
  graph
}
