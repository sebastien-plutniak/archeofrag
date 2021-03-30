\name{make_cr_graph}
\alias{make_cr_graph}
\alias{make_cr_graph-methods}
\title{
    Make a "connection" relations graph.
}
\description{
    Takes a \code{\link[=Frag.object-class]{frag.object}} and returns an undirected graph representing the "connection" relations between archaeological fragments. A "connection" relation refers to a physical connection between two fragments which were part of a same object.
}
\usage{
make_cr_graph(object)
}
\arguments{
  \item{object}{
  A \code{\link[=Frag.object-class]{frag.object}} object.
}

}
\details{Returns an undirected graph of "\code{igraph}" class. The "fragments" data frame of the frag.object is used to set the vertices attributes.}
\value{
An undirected \code{igraph} class graph. The "frag_type" graph attribute is set with the "connection" character value.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{\link{make_frag_object}}
\examples{
cr.df <- matrix(c(1,2, 1,3, 2,3, 4,5, 4,6, 7,8), ncol=2, byrow=TRUE)
sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
fragments.df <- data.frame(1:13, letters[1:13]) 

cr_g <- make_frag_object(cr=cr.df, fragments=fragments.df)
crsr_g <- make_frag_object(cr=cr.df, sr=sr.df, fragments=fragments.df)

make_cr_graph(cr_g)
make_cr_graph(crsr_g)
}
\keyword{graphs}

