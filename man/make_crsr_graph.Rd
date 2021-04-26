\name{make_crsr_graph}
\alias{make_crsr_graph}
\title{
    Makes a "connection" relationships graph including the "similarity" relationships.
}
\description{
    Takes a \code{\link[=Frag.object-class]{frag.object}} in argument and returns an undirected graph representing the relationships between archaeological fragments. "Connection" and "similiarity" relationships are combined. A "connection" relationship refers to a physical connection between two fragments that were part of the same object. A "similarity" relationship between fragments is defined if there is an acceptable likelihood that those fragments were part of the same object.
}
\usage{
    make_crsr_graph(object)
}
\arguments{
  \item{object}{A \code{\link[=Frag.object-class]{frag.object}}.}

}
\details{A complementary function to the \code{\link{make_cr_graph}} function. This function handles both the "connection" and "similarity" relationships.  This can be useful, given that "similarity" relations are more frequently documented in archaeological datasets than the "connection" relationships. 

The function returns an undirected graph of "\code{igraph}" class, using the "fragments" data frame of the frag.object to set the vertices attributes. 

Both "connection" and "similarity" relationships are included in the resulting graph. The edge attribute "type_relation" is set with a character "cr" value for "connection" relationships and with "sr" for "similarity" relationships. 
Edge weights are not set by this function, and it is recommended to use the \code{frag.edges.weighting} function. A "frag_type" graph attribute is set with a "connection and similarity" value.
}

\value{An undirected "igraph" class graph.}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{\link{make_frag_object}, \link{make_cr_graph}, \link{make_sr_graph}}

\examples{
cr.df <- matrix(c(1,2, 1,3, 2,3, 4,5, 4,6, 7,8), ncol=2, byrow=TRUE)
sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
fragments.df <- data.frame(1:13, letters[1:13]) 
crsr_g <- make_frag_object(cr=cr.df, sr=sr.df, fragments=fragments.df)
make_crsr_graph(crsr_g)
}
\keyword{build data}

