\name{frag.path.lengths}
\alias{frag.path.lengths}

\title{Path length distribution for unconnected graph}
\description{Path length distribution for unconnected graph}

\usage{frag.path.lengths(graph, cumulative=FALSE)}

\arguments{
  \item{graph}{An \code{igraph} object.} 
  \item{cumulative}{Logical. If TRUE, the cumulative relative frequency of the path lengths is returned.}
}
\details{
This function is a wrapper of \code{igraph} \code{distance_table} returning the distribution of paths lenght in an undirected and unconnected graphs. In the context of archaeological fragmentation analysis, path lenghts are used to characterise a layer from the fragmentation properties of the objects it contains.
}
\value{
A numeric vector having the same length than the maximum path length founded. The first element of the vector is the frequency of the paths of length 1, the second element is the frequence of the paths of length 2, etc. 
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
 
\keyword{graphs}
