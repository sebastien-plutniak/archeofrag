#   Frag.object CLASS-SPECIFIC FUNCTIONS ####

setGeneric(
  name= "make_crsr_graph",
  def = function(object){standardGeneric("make_crsr_graph")}
)


setGeneric(
  name= "make_cr_graph",
  def = function(object){standardGeneric("make_cr_graph")}
)

setGeneric(
  name= "make_sr_graph",
  def = function(object){standardGeneric("make_sr_graph")}
)

make_frag_set_validation <- function(object)
{
  if( ncol(object@cr.df) == 1 & ncol(object@sr.df) == 1 ){
    stop("At least one of the 'cr' and 'sr' arguments is required")
  }
  
  if( ncol(object@sr.df) > 1 ) {
    if(ncol(object@sr.df) < 2) {
      stop("The data frame for the 'sr' argument must have at least two columns")
    }
    if( sum( ! object@sr.df[,1] %in% object@fragments.df[, 1]) != 0){
      stop("Some objects in 'sr' are not documented in the 'fragments' data frame")
    }
  }

  if( ncol(object@cr.df) > 1 ) {
    if(ncol(object@cr.df) < 2) {
      stop("The data frame for the 'cr' argument must have at least two columns")
    }
    if( sum( ! c(object@cr.df[,1], object@cr.df[,2]) %in% object@fragments.df[, 1]) != 0){
      stop("Some objects in 'cr' are not documented in the 'fragments' data frame")
    }
  } 
 
  return(TRUE)
}


setClass(
  Class="Frag.object", 
  representation=representation(
    cr.df="matrix",
    sr.df="matrix",
    fragments.df="data.frame",
    frag_type="character"
  ),
  prototype = prototype(),
  validity = make_frag_set_validation
)

setMethod(
  f="show",
  signature = "Frag.object",
  definition = function (object){
    message(paste(
      "------ Frag.object ------",
      "\n*   Frag_type = ", object@frag_type,
      "\n*   N fragments = ",
      length(unique( na.omit(c(object@cr.df[,1],
                               object@cr.df[,2],
                               object@sr.df[,1]))) ),
      "\n-------------------------",
      sep=""))
  }
)

setMethod(
  f = "make_cr_graph",
  signature = "Frag.object",
  definition = function(object)
  {
    if( object@frag_type == "sr" ){
      stop("No available data for the connection relationships")
    }
    cr.net <- graph_from_data_frame(object@cr.df, directed=FALSE, vertices=object@fragments.df)
    cr.net <- delete_vertices(cr.net, degree(cr.net, mode="total") == 0)
    E(cr.net)$type <- "cr"
    cr.net <- set_graph_attr(cr.net, "frag_type", "connection relations")
    return(cr.net)
  }
)

setMethod(
  f = "make_sr_graph",
  signature = "Frag.object",
  definition = function(object)
  {
    if( object@frag_type == "cr" ){
      stop("No available data about the similarity relationships")
    }
    #  'similarity units' ids are recoded to avoid confusion with the fragments ids:
    object@sr.df[,2] <- as.character(factor(
      object@sr.df[,2], labels=paste("su", c(1:(0 + length(unique((object@sr.df[,2]))) )))
    ) )
    vertices.list <- unique( c(object@sr.df[, 1], object@sr.df[, 2]) ) 
    sr.net <- simplify(graph_from_data_frame(object@sr.df[, 1:2], directed=TRUE, vertices=vertices.list ))
    sr.net <- graph_from_adjacency_matrix(bibcoupling(sr.net), diag=FALSE, mode="undirected" )
    sr.net <- delete_vertices(sr.net, degree(sr.net, mode="total") == 0)
    # vertices attributes:
    fragments.df <- object@fragments.df
    names(fragments.df)[1] <- "name"
    
    attributes <- merge(                 #retrieve the  graph attributes
      cbind(name = V(sr.net)$name),
      cbind(fragments.df),
      by="name", sort=FALSE)
    vertex_attr(sr.net) <- lapply(attributes, as.character) #add vertex attributes
    # edge attribute:
    E(sr.net)$type <- "sr"
    return(sr.net)
  }
)

setMethod(
  f = "make_crsr_graph",
  signature = "Frag.object",
  definition = function(object)
  {
    if(object@frag_type != "crsr"){
      stop("The 'cr' and 'sr' arguments are required for this function")
    }
    cr.net <- make_cr_graph(object)
    sr.net <- make_sr_graph(object)
    
    crsr.net <- cr.net %u% sr.net
    crsr.list <- decompose(crsr.net) # get one graph for each component
    # union of each graph (ie: connection graph) with its complement graph (ie: similarity graph)
    crsr.list <- lapply(crsr.list,
                            function(x) x %u% complementer(x) )
    crsr.list <- lapply(crsr.list,
                            function(x){ set_vertex_attr(x, "name_save", V(x),  V(x)$name )} )
    # merge all the graphs in the list:
    crsr.net <- Reduce("union", crsr.list)
    
    # ATTRIBUTES ####  
    # 1. vertices attributes ####
    fragments.df <- object@fragments.df
    names(fragments.df)[1] <- "name"
    attributes <- merge(                 # retrieve the crsr.net graph attributes
      cbind(name = V(crsr.net)$name),
      cbind(fragments.df),
      by="name", sort=FALSE)
    vertex_attr(crsr.net) <- lapply(attributes, as.character) #add vertex attributes
    
    # 2. edge attributes ####
    edge.attributes(crsr.net) <- list()  # removing all edges attributes
    # we compare the edges in cr.net and crsr.net and only keep the "connection" edges
    # we got a list of the crsr.net edges also present in cr.net
    rename.edges <- function(edge.list){
      chain <- paste(edge.list[[1]], edge.list[[2]], sep="-")
      return(chain)
    }
    crsr.net.edgelist <- as_edgelist(crsr.net)
    crsr.net.edgelist <- apply(crsr.net.edgelist, 1, rename.edges)
    cr.net.edgelist <- as_edgelist(cr.net)
    cr.net.edgelist <- apply(cr.net.edgelist, 1, rename.edges)
    
    # setting the "type" edge attribute to "cr"
    E(crsr.net)$type <- NA
    E(crsr.net)[ which(crsr.net.edgelist %in% cr.net.edgelist) ]$type <- "cr"
    E(crsr.net)[ is.na( E(crsr.net)$type ) ]$type <- "sr"
    graph_attr(crsr.net) <- list()
    crsr.net <- set_graph_attr(crsr.net, "frag_type", "connection and similarity relations")
    
    return(crsr.net) 
  }
)


# Class constructor
make_frag_object <- function(cr, sr, fragments)
{
  if( missing(fragments) ){
      stop("A matrix or a data frame is required for the 'fragments' argument")
  } 
     
  if( ! missing(cr) & ! missing(sr) ){
    cr <- as.matrix(cr)
    sr <- as.matrix(sr)
    frag_type <- "crsr"
  } else if( ! missing(cr) ){
    cr <- as.matrix(cr)
    sr <- matrix()
    frag_type <- "cr"
  } else if( ! missing(sr) ){
    sr <- as.matrix(sr)
    cr <- matrix()
    frag_type <- "sr"
  }   
  
  new(Class="Frag.object", cr.df=cr, sr.df=sr, fragments.df=fragments, frag_type=frag_type)
}

