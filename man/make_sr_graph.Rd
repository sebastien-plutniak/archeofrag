\name{make_sr_graph}
\alias{make_sr_graph}
\alias{make_sr_graph-methods}
\title{
    Make a "similarity" relation graph.
}
\description{
    Takes a \code{\link[=Frag.object-class]{frag.object}} and returns an undirected graph representing the "similarity" relations between archaeological fragments. A "similarity" relation  between fragments is defined if there is an acceptable likelihood that those fragments were part of the same object.
}
\usage{
make_sr_graph(object)
}
\arguments{
  \item{object}{
  A \code{\link[=Frag.object-class]{frag.object}} object.
}

}
\details{Returns an undirected graph of "\code{igraph}" class. The "fragments" data frame of the frag.object is used to set the vertices attributes.}
\value{
An undirected \code{igraph} class graph. The "frag_type" graph attribute is set with the "similarity" character value.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{\link{make_frag_object}}

\examples{
sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
fragments.df <- data.frame(1:13, letters[1:13]) 
crsr_g <- make_frag_object(sr=sr.df, fragments=fragments.df)
make_sr_graph(crsr_g)
}
\keyword{graphs}

