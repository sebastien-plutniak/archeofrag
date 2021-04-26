\name{frag.cycles}
\alias{frag.cycles}
\title{
Count the k-cycles in a graph, for cycles =< k
}
\description{
Count the k-cycles in a graph, for cycles =< k
}
\usage{
frag.cycles(graph, kmax, max.cycles.only=FALSE)
}
\arguments{
  \item{graph}{An \code{igraph} object, must be an undirected graph.}
  \item{kmax}{Maximal length of the cycles to detect.}
  \item{max.cycles.only}{Logical. If TRUE, the fragments are only reported as parts of their longer cycle.}
}
\details{
A cycle can be part of larger cycle: if \code{max.cycles.only} all the cycles are reported but, if this parameter is True only the larger cycles are reported.
A warning recalls that for cycles k > 4 the fragments of a cycle are not necessarily all connected to each other (a fragment, due to its location in the original object, can only be connected to a limited number of adjacent fragments).
}
\value{
A data frame with the number of k-cycles for each k values in [3;k].
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
    \code{\link[igraph]{subgraph_isomorphisms}}
}

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.cycles(g, kmax=4, max.cycles.only=FALSE)
frag.cycles(g, kmax=4, max.cycles.only=TRUE)
}
\keyword{measurement}
