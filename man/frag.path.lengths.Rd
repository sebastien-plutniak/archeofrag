\name{frag.path.lengths}
\alias{frag.path.lengths}

\title{Path length distribution for unconnected graphs}
\description{Path length distribution for unconnected graphs}

\usage{frag.path.lengths(graph, cumulative=FALSE)}

\arguments{
  \item{graph}{An \code{igraph} object.} 
  \item{cumulative}{Logical. If TRUE, the cumulative relative frequency of the path lengths is returned.}
}
\details{
This function is a wrapper of \code{igraph} \code{distance_table} returning the frequency of path lengths in undirected and unconnected graphs. In the context of archaeological fragmentation analysis, path lengths are interpreted to characterise the properties of fragmentation within a layer.
}
\value{
A numeric vector having the same length as the maximum path length. The first element of the vector is the frequency of the paths of length 1, the second element is the frequency of the paths of length 2, etc. 
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
    \code{\link[igraph]{distance_table}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance = .15)
frag.path.lengths(g)
frag.path.lengths(g, cumulative=TRUE)
}
 
\keyword{measurement}
