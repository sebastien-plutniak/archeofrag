.euclidean.distance <- function(graph, x, y, z){
  coords <- cbind("name" = igraph::V(graph)$name,
                  "x" = igraph::vertex_attr(graph, x),
                  "y" = igraph::vertex_attr(graph, y),
                  "z" = igraph::vertex_attr(graph, z))
  e.list <- igraph::as_edgelist(graph)
  colnames(e.list) <- c("name1", "name2")
  e.list <- merge(e.list, coords, by.x="name1", by.y="name", sort=FALSE)
  e.list <- merge(e.list, coords, by.x="name2", by.y="name",
                  suffixes=c("1", "2"), sort=FALSE)
  e.list <- e.list[, order(colnames(e.list))]
  e.list <- e.list[, -c(1, 2)] 
  e.list <- apply(e.list, 2, as.numeric)
  e.list <- matrix(e.list, ncol = (ncol(coords)-1)*2, byrow=T)
  
  if(any(is.na(e.list))){
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
  e.list$id <- seq_len(nrow(e.list)) # add it to sort the row later
  colnames(e.list) <- c("name1", "name2", "id")
  morpho.df <- cbind("name" = igraph::V(graph)$name, 
                     "morphometry" = igraph::V(graph)$morphometry)
  e.list <- merge(e.list, morpho.df, by.x="name1", by.y="name", sort=FALSE)
  e.list <- merge(e.list, morpho.df, by.x="name2", by.y="name",
                  suffixes=c("1", "2"), sort=FALSE)
  e.list <- e.list[order(e.list$id), ] # sort the rows
  e.list <- e.list[, c(4, 5)]
  e.list <- apply(e.list, 2, as.numeric)
  e.list <- data.frame(matrix(e.list, ncol=2))
  colnames(e.list) <- c("morphometry1", "morphometry2")
  
  e.list$proportion <- apply(e.list, 1, function(xx){
    v <- sort(xx[c(1, 2)])
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

# compute morpho-spatial factor:
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
  v.list <- cbind("name" = igraph::V(g)$name,
                  "deg" = igraph::degree(g),
                  "trans" = igraph::transitivity(g, type = "localundirected",
                                       isolates = "zero", weights = NULL))
  e.list <- merge(e.list, v.list, by.x="src", by.y="name", sort=FALSE)
  e.list <- merge(e.list, v.list, by.x="tgt", by.y="name", sort=FALSE)
  e.list[, 3:6] <- apply(e.list[, 3:6], 2, as.numeric)
  
  e.list$SumDegree <- e.list$deg.x + e.list$deg.y
  e.list$trans.factor <- apply(e.list[, c(4,6)], 1, function(trans){
    3 - (2 / (1 + mean(trans)) )  
  })
  size.factor <- (1 - (1 / ( sqrt(igraph::gorder(g) + igraph::gsize(g)) ) ))^2
  e.list$SumDegree * e.list$trans.factor * size.factor
}




frag.edges.weighting <- function(graph, layer.attr, morphometry=NULL, x=NULL, y=NULL, z=NULL, verbose=TRUE){
  # TESTS: ----
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  
  # test morphometry ----
  igraph::V(graph)$morphometry <- 1 # set default value
  
  if(is.null(morphometry)){
    morphometry <- NULL
    } else if(verbose & ! is.character(morphometry)){
      stop("A character value is expected for the 'morphometry' parameter.")
    } else if( any(igraph::vertex_attr_names(graph) == morphometry) ){
      igraph::V(graph)$morphometry <- igraph::vertex_attr(graph, morphometry)
      morphometry <- "morphometry"
    } else if(verbose) {
      warning(paste("No '", morphometry, "' variable to use for morphometry, parameter skiped.", sep=""))
      morphometry <- NULL
    } else{
      morphometry <- NULL
    }
  
  if(verbose & ! is.numeric(igraph::V(graph)$morphometry)){
    warning("Numeric values are required for the 'morphometry' parameter, parameter skiped.")
    igraph::V(graph)$morphometry <- 1
    morphometry <- NULL
  }
  
  # test coordinates ----
  missing.coords <- c(x, y, z)[ ! c(x, y, z) %in% c("", igraph::vertex_attr_names(graph)) ]
  if(verbose & length(missing.coords) > 0){
    warning(paste0("No '", paste0(missing.coords, collapse = "', '"), "' variable(s) to use for coordinates, parameter skiped."))
  }
  
  if(is.null(x)){
    x <- ""
  } else if(verbose & ! is.character(x)){
    stop("A character value is expected for the 'x' parameter.")
  } else if( ! any(igraph::vertex_attr_names(graph) == x) ){
    x <- ""
  }
  
  if(is.null(y)){
    y <- ""
  } else if(verbose & ! is.character(y)){
    stop("A character value is expected for the 'y' parameter.")
  }  else if( ! any(igraph::vertex_attr_names(graph) == y) ){
    y <- ""
  }
  
  if(is.null(z)){
    z <- ""
  } else if(verbose & ! is.character(z)){
    stop("A character value is expected for the 'z' parameter.")
  }  else if( ! any(igraph::vertex_attr_names(graph) == z) ){
    z <- ""
  }
    
  
  # test spatial units ----
  layers <- igraph::vertex_attr(graph, layer.attr)
  layers.u <- unique(layers)
  if(verbose & length(layers.u) > 2){
    stop("There are more than two layers.")
  }
  
  
  # add unique identifiers:
  if(is.null(V(graph)$name)){
    igraph::V(graph)$name <- seq_len(igraph::gorder(graph))
  }
  
  igraph::E(graph)$id <- seq_len(igraph::gsize(graph))
  
  # generate subgraphs ----
  v1 <- layers == layers.u[1]
  v2 <- layers == layers.u[2]
  
  g1  <- igraph::subgraph_from_edges(graph, igraph::E(graph)[ igraph::V(graph)[v1] %--% igraph::V(graph)[v1] ])
  g2  <- igraph::subgraph_from_edges(graph, igraph::E(graph)[ igraph::V(graph)[v2] %--% igraph::V(graph)[v2] ])
  g12 <- igraph::subgraph_from_edges(graph, igraph::E(graph)[ igraph::V(graph)[v1] %--% igraph::V(graph)[v2] ])
  
  # extract edges indices:
  e1  <- igraph::E(graph)$id %in% igraph::E(g1)$id
  e2  <- igraph::E(graph)$id %in% igraph::E(g2)$id
  e12 <- igraph::E(graph)$id %in% igraph::E(g12)$id
  
  # compute weights for the edges of the 3 subgraphs: ----
  igraph::E(graph)[ e1 ]$weight <- .set.edge.weight(g1)
  igraph::E(graph)[ e2 ]$weight <- .set.edge.weight(g2)
  igraph::E(graph)[ e12]$weight <- .set.edge.weight(g12)
  
  # tags edges ----
  igraph::E(graph)$scope <- "intra"
  igraph::E(graph)[e12]$scope <- "extra"
  
  # RETURN ----
  # IF the morpho-spatial modifier is not required, return the result:
  if(is.null(morphometry) & all(c(x, y, z) == "")){ return(graph) }
  
  # ELSE, compute and apply the morpho-spatial modifier:
  # Morpho-spatial modifier ----
  
  # get the max morphometric/spatial values observed in the whole graph:
  params  <- .get.morpho.spatial.params(graph, x, y, z)
  if(all(params == 1)) {return(graph)}
  
  morph.max <- max(params$morphometry1 + params$morphometry2) 
  prop.max <- max(params$proportion)
  dist.max <- max(params$distance)
  
  # add a "distance" edge attribute if necessary:
  if(! is.null(params$distance)){
    igraph::E(graph)$distance <- params$distance
  }
  
  # get the morphometric/spatial parameters of the edges compute the 
  # morphometric/spatial factor for the edges  of each subgraphs:
  morpho.spatial.factor.g1 <- 0
  if(igraph::gsize(g1) > 0){
    params1  <- .get.morpho.spatial.params(g1, x, y, z)
    morpho.spatial.factor.g1 <- apply(params1, 1, function(xx){
      .morpho.spatial.factor(morpho1 = xx[1],
                             morpho2 = xx[2],
                             proportion = xx[3],
                             distance = xx[4],
                             morpho.max = morph.max,
                             proportion.max = prop.max,
                             distance.max = dist.max)
    })
  }
  
  morpho.spatial.factor.g2 <- 0
  if(igraph::gsize(g2) > 0){
    params2  <- .get.morpho.spatial.params(g2, x, y, z)
    morpho.spatial.factor.g2 <- apply(params2, 1, function(xx){
      .morpho.spatial.factor(morpho1 = xx[1],
                             morpho2 = xx[2],
                             proportion = xx[3],
                             distance = xx[4],
                             morpho.max = morph.max,
                             proportion.max = prop.max,
                             distance.max = dist.max)
    })
  }
  
  morpho.spatial.factor.g12 <- 0
  if(igraph::gsize(g12) > 0){
    params12 <- .get.morpho.spatial.params(g12, x, y, z)
    morpho.spatial.factor.g12 <- apply(params12, 1, function(xx){
      .morpho.spatial.factor(morpho1 = xx[1],
                             morpho2 = xx[2],
                             proportion = xx[3],
                             distance = xx[4],
                             morpho.max = morph.max,
                             proportion.max = prop.max,
                             distance.max = dist.max)
    })
  }
  
  igraph::E(graph)[ e1 ]$weight <- igraph::E(graph)[e1]$weight  * morpho.spatial.factor.g1
  igraph::E(graph)[ e2 ]$weight <- igraph::E(graph)[e2]$weight  * morpho.spatial.factor.g2
  igraph::E(graph)[ e12]$weight <- igraph::E(graph)[e12]$weight * morpho.spatial.factor.g12
  
  graph
}
