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
  if( ncol(object@df.cr) == 1 & ncol(object@df.sr) == 1 ){
    stop("At least one of the 'cr' and 'sr' arguments is required.")
  }
  if(  object@frag_type %in% c("sr", "crsr")) {
    if( ! ncol(object@df.sr) > 1){
      stop("The data frame for the 'sr' argument must have at least two columns.")
    }
    if( sum( ! object@df.sr[,1] %in% object@fragments.df[, 1]) != 0){
      stop("Some objects in the similarity relationships data ('sr' parameter) are not documented in fragments data ('fragments' parameter)")
    }
  }  

  if( object@frag_type %in% c("cr", "crsr") ) {
    if( ! ncol(object@df.cr) > 1){
      stop("The data frame for the 'cr' argument must have at least two columns.")
    }
    if( sum( ! c(object@df.cr[,1], object@df.cr[,2]) %in% object@fragments.df[, 1]) != 0){
      stop("Some objects in the connection relationships data ('cr' parameter) are not documented in fragments data ('fragments' parameter)")
    }
  } 
 
  return(TRUE)
}

 


setClass(
  Class="Frag.object", 
  representation=representation(
    df.cr="matrix",
    df.sr="matrix",
    fragments.df="data.frame",
    frag_type="character"
  ),
  prototype = prototype(),
  validity = make_frag_set_validation
)

setMethod( # show  ----
  f="show",
  signature = "Frag.object",
  definition = function (object){
    message(paste(
      "------ Frag.object ------",
      "\n*   Frag_type = ", object@frag_type,
      "\n*   N fragments = ",
      length(unique( stats::na.omit(c(object@df.cr[,1],
                                      try(object@df.cr[,2], silent=TRUE),
                                      object@df.sr[,1]))) ),
      "\n-------------------------",
      sep=""))
  }
)

setMethod(  # make_cr_graph ----
  f = "make_cr_graph",
  signature = "Frag.object",
  definition = function(object)
  {
    if( object@frag_type == "sr" ){
      stop("No data available for connection relationships.")
    }
    cr.net <- igraph::graph_from_data_frame(object@df.cr, directed=FALSE,
                                            vertices=object@fragments.df)
    cr.net <- igraph::delete_vertices(cr.net, 
                                      igraph::degree(cr.net, mode="total") == 0)
    cr.net <- simplify(cr.net, remove.multiple = TRUE)
    igraph::E(cr.net)$type_relation <- "cr"
    cr.net <- igraph::set_graph_attr(cr.net, "frag_type", "connection relations")
    return(cr.net)
  }
)

setMethod(  # make_sr_graph ----
  f = "make_sr_graph", 
  signature = "Frag.object",
  definition = function(object)
  {
    
    if( object@frag_type == "cr" ){
      stop("No data available for similarity relationships.")
    }
    #  'similarity units' ids are recoded to avoid confusion with the fragments ids:
    object@df.sr[,2] <- as.character(factor(object@df.sr[,2], 
      labels = paste("su", c(1:(0 + length(unique((object@df.sr[,2]))) )))
    ) )
    vertices.list <- unique( c(object@df.sr[, 1], object@df.sr[, 2]) ) 
    sr.net <- igraph::simplify(igraph::graph_from_data_frame(object@df.sr[, 1:2], 
                                                 directed=TRUE, vertices=vertices.list ))
    sr.net <- igraph::graph_from_adjacency_matrix(igraph::bibcoupling(sr.net),
                                                  diag=FALSE, mode="undirected" )
    sr.net <- igraph::delete_vertices(sr.net, igraph::degree(sr.net, mode="total") == 0)
    # vertices attributes:
    fragments.df <- object@fragments.df
    names(fragments.df)[1] <- "name"
    
    attributes <- merge(       #retrieve the  graph attributes
      cbind("name" = igraph::V(sr.net)$name),
      cbind(fragments.df),
      by="name", sort=FALSE)
    igraph::vertex_attr(sr.net) <- lapply(attributes, as.character) #add vertex attributes
    
    # clean graph:
    sr.net <- igraph::delete_vertices(sr.net, igraph::degree(sr.net) == 0)
    # Set attributes:
    igraph::E(sr.net)$type_relation <- "sr"
    sr.net <- igraph::set_graph_attr(sr.net, "frag_type", "similarity relations")
    return(sr.net)
  }
)

setMethod( # make_crsr_graph ----
  f = "make_crsr_graph",
  signature = "Frag.object",
  definition = function(object)
  {
    if(object@frag_type != "crsr"){
      stop("Connection and similarity relationships are required for this function ('cr' and 'sr' arguments).")
    }
    
    cr.net <- make_cr_graph(object)
    sr.net <- make_sr_graph(object)
    
    crsr.net <- igraph::union(cr.net, sr.net)
    crsr.list <- igraph::decompose(crsr.net) # get one graph for each component
    # union of each graph (ie: connection graph) with its complement graph (ie: similarity graph)
    crsr.list <- lapply(crsr.list,
                            function(x) igraph::union(x, igraph::complementer(x)) )
    crsr.list <- lapply(crsr.list,
                            function(x){ igraph::set_vertex_attr(x, "name_save", 
                                                         igraph::V(x),  igraph::V(x)$name )} )
    # merge all the graphs in the list:
    crsr.net <- Reduce(igraph::union, crsr.list)
    
    # ATTRIBUTES ----
    # 1. vertices attributes ####
    fragments.df <- object@fragments.df
    names(fragments.df)[1] <- "name"
    attributes <- merge(                 # retrieve the crsr.net graph attributes
      cbind("name" = igraph::V(crsr.net)$name),
      cbind(fragments.df),
      by="name", sort=FALSE)
    igraph::vertex_attr(crsr.net) <- lapply(attributes, as.character) #add vertex attributes
    
    # 2. edge attributes ####
    igraph::edge_attr(crsr.net) <- list()  # removing all edges attributes
    # we compare the edges in cr.net and crsr.net and only keep the "connection" edges
    # we got a list of the crsr.net edges also present in cr.net
    rename.edges <- function(edge.list){
      chain <- paste(edge.list[[1]], edge.list[[2]], sep="-")
      return(chain)
    }
    crsr.net.edgelist <- igraph::as_edgelist(crsr.net)
    crsr.net.edgelist <- apply(crsr.net.edgelist, 1, rename.edges)
    cr.net.edgelist <- igraph::as_edgelist(cr.net)
    cr.net.edgelist <- apply(cr.net.edgelist, 1, rename.edges)
    
    # setting the "type_relation" edge attribute to "cr"
    igraph::E(crsr.net)$type_relation <- NA
    igraph::E(crsr.net)[ which(crsr.net.edgelist %in% cr.net.edgelist) ]$type_relation <- "cr"
    igraph::E(crsr.net)[ is.na( igraph::E(crsr.net)$type_relation ) ]$type_relation <- "sr"
    igraph::graph_attr(crsr.net) <- list()
    crsr.net <- igraph::set_graph_attr(crsr.net, "frag_type", "connection and similarity relations")
    
    return(crsr.net) 
  }
)


# Class constructor
make_frag_object <- function(cr, sr, fragments)
{
  if( missing(fragments) ){
    stop("A matrix or a data frame is required for the 'fragments' parameter.")
  } 
     
  if( ! missing(cr) & ! missing(sr) ){
    cr <- as.matrix(cr)
    cr <- apply(cr, 2, as.character)
    sr <- as.matrix(sr)
    sr <- apply(sr, 2, as.character)
    frag_type <- "crsr"
  } else if( ! missing(cr) ){
    cr <- as.matrix(cr)
    cr <- apply(cr, 2, as.character)
    sr <- matrix()
    frag_type <- "cr"
  } else if( ! missing(sr) ){
    sr <- as.matrix(sr)
    sr <- apply(sr, 2, as.character)
    cr <- matrix()
    frag_type <- "sr"
  }   
  
  fragments[, 1] <- as.character(fragments[, 1])
  
  new(Class="Frag.object", df.cr=cr, df.sr=sr, fragments.df=fragments, frag_type=frag_type)
}

