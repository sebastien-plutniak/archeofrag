.subsetsum <- function(x, target, i = 1){
  while(i != length(x)){
    s <- sum(x[1:i], na.rm = T)
    if(s==target) break
    if(s > target) x[i] <- NA
    i <- i + 1
  }
  x[seq(i+1, length(x))] <- NA
  which(! is.na(x))
}

.select.component <- function(g, aggreg.factor){
  # high aggreg.factor favors biggest components:
  proba <- 1/ (1 + (1:clusters(g)$no * aggreg.factor))
  sample(order(clusters(g)$csize, decreasing = T), 1, prob = proba)
}

.connect.neighbors.if.planar <- function(g, v, v.to.add.name, neighbors){
  g.tmp <- induced_subgraph(g, V(g)[ V(g)$object.id == V(g)[v]$object.id ])
  g.tmp <- add_vertices(g.tmp, 1, attr = list(name = v.to.add.name))
  g.tmp <- add_edges(g.tmp, as.character(c(rbind(v.to.add.name, neighbors)) ))
  
  if(boyerMyrvoldPlanarityTest(as_graphnel(g.tmp))){
    g <- add_edges(g, c(rbind(v.to.add.name, neighbors)) )
  }
  g    
}

.connect.neighbors <- function(g, v, v.to.add.name, edges, planar){
  # identify the neighbors of the target vertex:
  neighbors <- neighborhood(g, order = 1, nodes = v, mindist=1)
  neighbors <- unlist(neighbors)
  neighbors <- neighbors[ ! neighbors == v.to.add.name]
  neighbors <- as.character(neighbors)
  neighbors <- sample(neighbors, sample(0:length(neighbors), 1) )
  # if there are other neighbours than the new vertex, try connection:
  if(length(neighbors) > 0 & (gsize(g) + length(neighbors)) < edges ){
    if(planar){
      g <- .connect.neighbors.if.planar(g, v, v.to.add.name, neighbors)
    }else{
      g <- add_edges(g, c(rbind(v.to.add.name, neighbors)) ) # planarity test
    }
  }
  g
}

.add.fragment <- function(g, n.components, edges, connect.neighbors, planar, aggreg.factor){
  # select component:
  compo.to.fragment <- .select.component(g, aggreg.factor)
  v.to.fragment <- V(g)[V(g)$object.id == compo.to.fragment]$name
  # select the target vertex in the component:
  v <-  sample(as.character(v.to.fragment), 1)
  # add a new vertex and an edge with the target vertex:
  v.to.add.name <- gorder(g) + 1
  g <- add_vertices(g, 1, attr = list(name = v.to.add.name, 
                                      object.id = V(g)[v]$object.id ))
  g <- add_edges(g, c(v, v.to.add.name))
  
  # try to connect the new vertex and the neighbors of the target vertex:
  if(connect.neighbors){
    g <- .connect.neighbors(g, v, v.to.add.name, edges, planar)
  }
  g
}


.main <- function(n.components, vertices, edges, balance, disturbance, aggreg.factor, planar){
  # Initialize graph:  
  g <- n.components * make_graph(c(1, 2), directed = FALSE)
  V(g)$name <- 1:gorder(g)
  V(g)$object.id <- clusters(g)$membership
  
  # Build graph:
  if(is.infinite(vertices)){ # only edge count constraint
    if(vertices/2 < n.components){
      stop("Increase 'vertices' or decrease 'n.components'.")}
    while(gsize(g) < edges){
      g <- .add.fragment(g, n.components, edges, connect.neighbors=T, planar, aggreg.factor)
    }
  } else if(is.infinite(edges)){ # only vertices count constraint
    if(edges < n.components){
      stop("Increase 'edges' or decrease 'n.components'.")}
    while(gorder(g) < vertices){
      g <- .add.fragment(g, n.components, edges, connect.neighbors=T, planar, aggreg.factor)
    }
  } else if(! is.infinite(vertices) & ! is.infinite(edges) ){ # vertices & edges constraints
    if(edges < n.components + (vertices - 2 * n.components) ){
      stop("Irrelevant parameters, decrease 'vertices' or increase 'edges'.") 
    }   # if not enough edges to add vertices:
    if(vertices - n.components * 2 >  edges - n.components ){
      stop("Irrelevant parameters, decrease 'vertices' or increase 'edges'.")
    }
    # if(edges > 3 * (vertices - (n.components - 1)*2) - 7 + n.components){ # uncorrect, don't work for graphs only made of dyads
    if(edges > 3 * (vertices - (n.components - 1)*2) - 6 + n.components){ # adapted version, but probably uncorrect
      stop("Irrelevant parameters, decrease 'edges' or increase 'vertices'.")
    }  
    
    while(gorder(g) < vertices & gsize(g) < edges ){
      g <- .add.fragment(g, n.components, edges, connect.neighbors=F, planar, aggreg.factor)
    }
    # check if the number of supplementary edges can conserve planarity:
    e.existing <- 0 # initialize the value
    e.max <- edges  # initialize the value
    clus <- clusters(g)$csize
    gsub <- induced_subgraph(g,
                             V(g)[ clusters(g)$membership %in% which(clus > 2)])
    gsub <- decompose(gsub)
    if(length(gsub) > 0){
      e.existing <- sapply(gsub, gsize)
      e.max <- sapply(clus[clus > 2], function(x)  3*x-6) # e max for planar graphs
      e.max <- sum(e.max - e.existing)
    }
    
    if(edges - gsize(g) > e.max){
      # message("Few or no solution possible for these parameters. Consider changing them. The graph generated has less edges than the 'edges' parameter.")
      # edges <- gsize(g) + e.max # the nr of edges is reduced
      stop("No solution for these parameters, given the current random result. Consider removing the edge or vertex constraint.")
    }
    while(gsize(g) < edges){
      # select a component:
      selected.component <- .select.component(g, aggreg.factor)
      v.to.connect <- V(g)[V(g)$object.id == selected.component]$name
      # select two vertices in the component:
      v.to.connect <-  sample(as.character(v.to.connect), 2)
      if(length(E(g)[v.to.connect[1] %--% v.to.connect[2]]) == 0){# if edge not exists
        # check planarity before adding edge:
        if(planar){
          g.tmp <- induced_subgraph(g, V(g)[ V(g)$object.id == selected.component])
          g.tmp <- add_edges(g.tmp, v.to.connect)
          
          if(boyerMyrvoldPlanarityTest(as_graphnel(g.tmp))){
            g <- add_edges(g, v.to.connect)
          }
        }else{
          g <- add_edges(g, v.to.connect)
        }
      }
    }
    
  }
  g
}


.add.disturbance <- function(g, v.to.disturb, asymetric.transport.from){
  # default behaviour:
  v.to.disturb <- sample(seq(1, gorder(g)), v.to.disturb)
  # if asymetric.transport.from is set:
  if( ! is.null(asymetric.transport.from)){
    if(length(v.to.disturb) <= length(V(g)[V(g)$layer == asymetric.transport.from])   ){
      v.to.disturb <- sample(V(g)[V(g)$layer == asymetric.transport.from], v.to.disturb)
    } else{
      stop("The number of fragments for asymetric transport exceeds the number of fragments in this layer. Reduce the 'disturbance' value.")
    }
  } 
  V(g)[v.to.disturb]$layer <- as.character(factor(V(g)[v.to.disturb]$layer,
                                                  levels = c(1,2), labels = c(2,1)))
  g
}



#'	@export
frag.simul.process <- function(initial.layers=2, n.components, vertices=Inf, edges=Inf, balance=.5, components.balance=.5, disturbance=0, aggreg.factor=0, planar=TRUE, asymetric.transport.from=NULL, from.observed.graph=NULL, observed.layer.attr=NULL){
  
  #  If requested input parameters from observed graph (except the number of edges):
  if( ! is.null(from.observed.graph) & ! is.null(observed.layer.attr)){
    if( ! is.character(observed.layer.attr))  stop("The parameter 'observed.layer.attr' requires a character value.")
    if( ! observed.layer.attr %in% names(vertex_attr(from.observed.graph)) ){
      stop(paste("No '", observed.layer.attr, "' vertices attribute", sep=""))
    }
    if(length(unique(vertex_attr(from.observed.graph, observed.layer.attr))) != 2){
      stop("The layer attribute of the observed graph must contain two layers.")
    }
    # run the get.parameters function:
    params <- frag.get.parameters(from.observed.graph, observed.layer.attr)
    # input the observed parameters:
    n.components <- params$n.components
    vertices <- params$vertices
    balance <- params$balance
    components.balance <- params$components.balance
    disturbance <- params$disturbance
    aggreg.factor <- params$aggreg.factor
    planar <- params$planar
  }
  
  # BEGIN Tests:
  if(! is.logical(planar)) stop("The 'planar' argument must be logical.")
  if(is.null(n.components)) stop("The 'n.components' parameter is required.")
  
  if(! is.numeric(balance)){
    stop("The 'balance' argument requires a numerical value.")
  } else if(balance <= 0 | balance >= 1){
    stop("'balance' values must range in ]0;1[")
  }
  
  if(! is.numeric(disturbance)){
    stop("The 'disturbance' argument requires a numerical value.")
  } else if(disturbance < 0 | disturbance > 1){
    stop("'disturbance' values must range in [0;1].")
  }
  
  if(is.infinite(vertices) & is.infinite(edges)){
    stop("At least one of the parameters 'vertices' or 'edges' is required.")
  }
  if(! initial.layers %in% c(1, 2)){
    stop("The 'initial.layers' parameter requires a numerical value of 1 or 2.")
  }
  
  if(! is.numeric(aggreg.factor)){
    stop("The 'disturbance' argument requires a numerical value.")
  } else if(aggreg.factor > 1 | aggreg.factor < 0 ){
    stop("The 'aggreg.factor' parameter must range in [0;1].")
  }
  
  if( ! is.null(asymetric.transport.from) ){
    if(! asymetric.transport.from %in% c(1, 2, "1", "2")){
      stop("The 'asymetric.transport.from' parameter must have a value in 1 or 2.")
    } 
  }
  
  # END tests.
  
  # BEGIN main body of the function:
  
  if(initial.layers == 1){
    g <- .main(n.components, vertices, edges, balance, disturbance, aggreg.factor, planar)
    
    # BALANCE. Determine layer size:
    v.layer1 <- floor(gorder(g) * balance)
    
    # search possible combinations of components and use the first one:
    sel.clusters <- clusters(g)$csize
    names(sel.clusters) <- seq_len(length(sel.clusters))
    sel.clusters <- .subsetsum(sample(sel.clusters), v.layer1) # randomize order
    sel.clusters <- names(sel.clusters)
    
    # assign layers:
    V(g)$layer <- 2
    V(g)[ V(g)$object.id %in% sel.clusters ]$layer <- 1
    
    # ADD DISTURBANCE:
    v.to.disturb <- round(gorder(g) * disturbance)
    if(v.to.disturb != 0){
      g <- .add.disturbance(g, v.to.disturb, asymetric.transport.from)
    }
  }
  
  if(initial.layers == 2){
    if(! is.infinite(edges)){message("The 'edge' parameter is not used if two 'initial layers' are used.")}
    n.components.l1 <- round(n.components * components.balance)
    n.components.l2 <- n.components - n.components.l1
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
    V(g.layer1)$layer <- 1
    V(g.layer2)$layer <- 2
    V(g.layer2)$name <- paste(V(g.layer2)$name, ".2", sep="")
    g <- g.layer1 %du% g.layer2
    # ADD DISTURBANCE:
    v.to.disturb <- round(gorder(g) * disturbance)
    if(v.to.disturb != 0){
      g <- .add.disturbance(g, v.to.disturb, asymetric.transport.from)
    }
  }
  g <- frag.edges.weighting(g, layer.attr="layer")
  g <- delete_vertex_attr(g, "which")
  g$frag_type <- "cr"
  g
}
