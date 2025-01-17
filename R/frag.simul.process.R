.subsetsum <- function(x, target, i = 1){
  while(i != length(x)){
    s <- sum(x[1:i], na.rm = T)
    if(s == target) break
    if(s > target) x[i] <- NA
    i <- i + 1
  }
  x[seq(i+1, length(x))] <- NA
  which(! is.na(x))
}

.select.component <- function(g, aggreg.factor){
  # high aggreg.factor favors bigger components:
  proba <- 1/ (1 + (1:igraph::components(g)$no * aggreg.factor))
  sample(order(igraph::components(g)$csize, decreasing = T), 1, prob = proba)
}

.connect.neighbors <- function(g, v, v.to.add.name, edges, planar){
  # if there are other neighbours than the new vertex, try to connect one of them with the new vertex.
  # identify the neighbors of the target vertex:
  neighbors <- igraph::ego(g, order = 1, nodes = v, mindist = 1)
  neighbors <- unlist(neighbors)
  neighbors <- neighbors[ ! neighbors == v.to.add.name]
  neighbors <- as.character(neighbors)
  # select none or one of the neighbors:
  neighbors <- sample(neighbors, sample(0:length(neighbors), 1))
  if(length(neighbors) > 0 & (igraph::gsize(g) + 1) < edges ){
    if(planar){
      # make a temporary version of the modified component of the graph:
      g.tmp <- igraph::induced_subgraph(g, igraph::V(g)[ igraph::V(g)$object.id == igraph::V(g)[v]$object.id ])
      g.tmp <- igraph::add_edges(g.tmp, as.character(c(rbind(v.to.add.name, neighbors)) ))
      # if the modified component is still planar, then connect the new vertex and the neighbor: 
      if(RBGL::boyerMyrvoldPlanarityTest(igraph::as_graphnel(g.tmp))){
        g <- igraph::add_edges(g, as.character(c(rbind(v.to.add.name, neighbors)) ))
      }
    }else{ # if no planarity constraint:
      g <- igraph::add_edges(g, c(rbind(v.to.add.name, neighbors)) )
    }
  }
  g
}

.add.fragment <- function(g, n.components, edges, connect.neighbors, planar, aggreg.factor){
  # select component:
  compo.to.fragment <- .select.component(g, aggreg.factor)
  
  v.to.fragment <- igraph::V(g)[ igraph::V(g)$object.id == compo.to.fragment]$name
  # select the target vertex in the component:
  v <-  sample(as.character(v.to.fragment), 1)
  # add a new vertex and an edge with the target vertex:
  v.to.add.name <- igraph::gorder(g) + 1
  g <- igraph::add_vertices(g, 1, attr = list("name" = v.to.add.name, 
                                      "object.id" = igraph::V(g)[v]$object.id ))
  g <- igraph::add_edges(g, c(v, v.to.add.name))
  
  # try to connect the new vertex and the neighbors of the target vertex:
  if(connect.neighbors){
    g <- .connect.neighbors(g, v, v.to.add.name, edges, planar)
  }
  g
}




.main <- function(n.components, vertices, edges, balance, disturbance, aggreg.factor, planar){
  stop.msg <- "No solution with these parameters, decrease the number of 'edges', increase the number of 'vertices', or remove one of these constraints."
  # Initialize graph:  
  g <- n.components * igraph::make_graph(c(1, 2), directed = FALSE)
  igraph::V(g)$name <- 1:igraph::gorder(g)
  igraph::V(g)$object.id <- igraph::components(g)$membership
  # Build graph:
  if(is.infinite(vertices)){ # only edge count constraint
    if(vertices/2 < n.components){
      stop(stop.msg)}
    while(igraph::gsize(g) < edges){
      g <- .add.fragment(g, n.components, edges, connect.neighbors=T, planar, aggreg.factor)
    }
  } else if(is.infinite(edges)){ # only vertices count constraint
    if(edges < n.components){
      stop("Increase 'edges' or decrease 'n.components'.")}
    while(igraph::gorder(g) < vertices){
      g <- .add.fragment(g, n.components, edges, connect.neighbors=T, planar, aggreg.factor)
    }
  } else if(! is.infinite(vertices) & ! is.infinite(edges) ){ # vertices & edges constraints
    if(edges < n.components + (vertices - 2 * n.components) ){
      stop(stop.msg) 
    }   # if not enough edges to add vertices:
    if(vertices - n.components * 2 >  edges - n.components ){
      stop(stop.msg)
    }
    # if(edges > 3 * (vertices - (n.components - 1)*2) - 7 + n.components){ # uncorrect, don't work for graphs only made of dyads
    if(edges > 3 * (vertices - (n.components - 1)*2) - 6 + n.components){ # adapted version, but probably uncorrect
      stop(stop.msg)
    }  
    
    while(igraph::gorder(g) < vertices & igraph::gsize(g) < edges ){
      g <- .add.fragment(g, n.components, edges, connect.neighbors=F, planar, aggreg.factor)
    }
    # check if the number of supplementary edges conserve planarity:
    e.existing <- 0 # initialize the value
    e.max <- edges  # initialize the value
    components.size <- igraph::components(g)$csize
    gsub <- igraph::induced_subgraph(g,
                             igraph::V(g)[ igraph::components(g)$membership %in% which(components.size > 2)])
    gsub <- igraph::decompose(gsub)
    if(length(gsub) > 0){
      e.existing <- sapply(gsub, igraph::gsize)
      e.max <- sapply(components.size[components.size > 2], function(x)  3*x-6) # e max for planar graphs
      e.max <- sum(e.max - e.existing)
    }
    
    if(edges - igraph::gsize(g) > e.max){
      stop(stop.msg)
    }
    while(igraph::gsize(g) < edges){
      # select a component:
      selected.component <- .select.component(g, aggreg.factor)
      v.to.connect <- igraph::V(g)[ igraph::V(g)$object.id == selected.component]$name
      # select two vertices in the component:
      v.to.connect <-  sample(as.character(v.to.connect), 2)
      if(length(igraph::E(g)[v.to.connect[1] %--% v.to.connect[2]]) == 0){# if the edge does not exist yet
        # check planarity of the component before adding edge:
        if(planar){
          g.tmp <- igraph::induced_subgraph(g, igraph::V(g)[ igraph::V(g)$object.id == selected.component])
          g.tmp <- igraph::add_edges(g, v.to.connect)
          
          if(RBGL::boyerMyrvoldPlanarityTest(igraph::as_graphnel(g.tmp))){
            g <- igraph::add_edges(g, v.to.connect)
          }
        }else{
          g <- igraph::add_edges(g, v.to.connect)
        }
      }
    } # end of the while loop
  } # end of the elseif "vertices + edges constraints"
  g
}


.add.disturbance <- function(g, nr.v.to.disturb, asymmetric.transport.from){
  # default behaviour:
  v.to.disturb <- sample(seq(1, igraph::gorder(g)), nr.v.to.disturb)
  # if asymmetric.transport.from is set:
  if(asymmetric.transport.from != 0){
    if(nr.v.to.disturb <= length(igraph::V(g)[ igraph::V(g)$layer == asymmetric.transport.from]) ){
      v.to.disturb <- sample(igraph::V(g)[ igraph::V(g)$layer == asymmetric.transport.from], nr.v.to.disturb)
    } else{
      stop("The number of fragments for asymmetric transport exceeds the number of fragments in this layer. Decrease the 'disturbance' value.")
    }
  } 
  # reverse layer values of the selected vertices:
  igraph::V(g)[v.to.disturb]$layer <- as.character(factor(igraph::V(g)[v.to.disturb]$layer,
                                                  levels = c(1,2), labels = c(2,1)))
  g
}


 

frag.simul.process <- function(initial.layers=2, n.components=NULL, vertices=Inf, edges=Inf, balance=.5, components.balance=.5, disturbance=0, aggreg.factor=0, planar=FALSE, asymmetric.transport.from=NULL, from.observed.graph=NULL, observed.layer.attr=NULL){
  
  if(! is.logical(planar)) stop("The 'planar' argument must be logical.")
  if(planar==TRUE & (! requireNamespace("RBGL", quietly=TRUE))){
    stop("To use the `planar` constraint, the RBGL package is required.")
  }
  
  #  If required by the user, use parameters from the observed graph (except the number of edges):
  if( ! is.null(from.observed.graph)){
    if ( is.null(observed.layer.attr) ) stop("The 'observed.layer.attr' parameter is missing.")
    if( ! is.character(observed.layer.attr))  stop("The parameter 'observed.layer.attr' requires a character value.")
    if( ! observed.layer.attr %in% names(igraph::vertex_attr(from.observed.graph)) ){
      stop(paste("No '", observed.layer.attr, "' vertices attribute.", sep=""))
    }
    if(length(unique(igraph::vertex_attr(from.observed.graph, observed.layer.attr))) != 2){
      stop("The `layer` attribute of the observed graph must contain two layers.")
    }
    # retrieve the observed graph's values:
    params <- frag.get.parameters(from.observed.graph, observed.layer.attr)
    # set the parameters with observed values if not already set by the user:
    if(missing(n.components)) n.components <- params$n.components
    if(missing(vertices)) vertices <- params$vertices
    if(missing(balance)) balance <- params$balance
    if(missing(components.balance)) components.balance <- params$components.balance
    if(missing(disturbance)) disturbance <- params$disturbance
    if(missing(aggreg.factor)) aggreg.factor <- params$aggreg.factor
    if(missing(planar))  planar <- params$planar
    if(is.na(planar)) {
      planar <- FALSE
      warning("The planarity of the graph value is indeterminated, simulations are executed with no planar constraint.")
    }
  }
  # BEGIN Tests:
  if(is.null(n.components)) stop("The 'n.components' parameter is required.")

  if(! is.numeric(balance)){
    stop("The 'balance' argument requires a numerical value.")
  } else if(balance <= 0 | balance >= 1){
    stop("'balance' values must range in ]0;1[")
  }

  if(! is.numeric(components.balance)){
    stop("The 'components.balance' argument requires a numerical value.")
  } else if(components.balance <= 0 | components.balance >= 1){
    stop("'components.balance' values must range in ]0;1[")
  }

  if(! is.numeric(disturbance)){
    stop("The 'disturbance' argument requires a numerical value.")
  } else if(disturbance < 0 | disturbance > 1){
    stop("'disturbance' values must range in [0;1].")
  }

  if(is.infinite(vertices) & is.infinite(edges)){
    stop("At least one of the parameters 'vertices' or 'edges' is required.")
  }
  if(is.infinite(vertices) & initial.layers == 2 ){
    stop("With 2 initial layers, the vertices parameter is required.")
  }
  
  if(! initial.layers %in% c(1, 2)){
    stop("The 'initial.layers' parameter requires a numerical value of 1 or 2.")
  }

  if(! is.numeric(aggreg.factor)){
    stop("The 'disturbance' argument requires a numerical value.")
  } else if(aggreg.factor > 1 | aggreg.factor < 0 ){
    stop("The 'aggreg.factor' parameter must range in [0;1].")
  }

  if(missing(asymmetric.transport.from)) asymmetric.transport.from <- 0
  if(! asymmetric.transport.from %in% c(0, 1, 2, "0" ,"1", "2")){
    stop("Values for 'asymmetric.transport.from' must be one of 1, 2 or 0.")
  }
  
  if(n.components > vertices / 2){
      stop("'n.components' must be <= 'vertices' / 2.")
  }  
  # END tests
  
  # BEGIN main body of the function:
  
  if(initial.layers == 1){ # for 1 initial layer ----
    g <- .main(n.components, vertices, edges, balance, disturbance, aggreg.factor, planar) 
    
    # BALANCE. Determine layer size:
    v.layer1 <- round(igraph::gorder(g) * balance)
    
    # search possible combinations of components and use the first one:
    sel.components <- igraph::components(g)$csize
    names(sel.components) <- seq_len(length(sel.components))
    sel.components <- .subsetsum(sample(sel.components), v.layer1) # randomize order
    sel.components <- names(sel.components)
    
    # assign layers:
    igraph::V(g)$layer <- 2
    igraph::V(g)[ igraph::V(g)$object.id %in% sel.components ]$layer <- 1
    
    # ADD DISTURBANCE:
    nr.v.to.disturb <- round(igraph::gorder(g) * disturbance)
    if(nr.v.to.disturb > 0){
      g <- .add.disturbance(g, nr.v.to.disturb, asymmetric.transport.from)
    }
  }
  
  if(initial.layers == 2){ # for 2 initial layers ----
    if(! is.infinite(edges)){
      warning("With two initial layers, the 'edge' parameter is not used.")
    }
    n.components.l1 <- round(n.components * components.balance)
    n.components.l2 <- n.components - n.components.l1
    if(n.components.l1 == 0 | n.components.l2 == 0){
      stop("The 'components.balance' is too low or too high.")
    }
    vertices.l1 <- round(vertices * balance)
    vertices.l2 <- vertices - vertices.l1
    
    g.layer1 <- .main(n.components.l1,
                      vertices.l1,  
                      edges = Inf,
                      balance = .5,
                      disturbance = 0,
                      aggreg.factor,
                      planar)
    g.layer2 <- .main(n.components.l2,
                      vertices.l2,
                      edges = Inf,
                      balance = .5,
                      disturbance = 0,
                      aggreg.factor,
                      planar) 
    # mark and merge the two graphs:
    igraph::V(g.layer1)$layer <- 1
    igraph::V(g.layer2)$layer <- 2
    igraph::V(g.layer2)$name <- paste(igraph::V(g.layer2)$name, ".2", sep="")
    g <- igraph::disjoint_union(g.layer1, g.layer2)
    # ADD DISTURBANCE:
    nr.v.to.disturb <- round(igraph::gorder(g) * disturbance)
    if(nr.v.to.disturb > 0){
      g <- .add.disturbance(g, nr.v.to.disturb, asymmetric.transport.from)
    }
  }
  # finalize and return the graph:
  g <- frag.edges.weighting(g, layer.attr="layer")
  g <- igraph::delete_vertex_attr(g, "which")
  g$frag_type <- "cr"
  g
}
