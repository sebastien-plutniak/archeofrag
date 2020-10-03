\name{frag.diameters}
\alias{frag.diameters}
\title{Diameter distribution for unconnected graph}
\description{
Returns the distribution of the diameter values of an unconnected graph.
}
\usage{
frag.diameters(graph, cumulative = FALSE)
}
\arguments{
  \item{graph}{An \code{igraph} object.}
  \item{cumulative}{Logical. If TRUE the cumulative relative frequency of the diameters is reported.}
}
\details{
\code{frag.diameters} wraps the \code{igraph} \code{diameter} function.
For graphs representing the fragmentation of archeological objects, the diameter of each components of the graph (i.e. archeological objects) can be interpreted:
as a measure of the insensity of the fragmentation (when all the fragments of the initial object are known);
as a measure of the scattering of the fragments (when not all the fragments are known);
}
\value{
A numeric vector of the length equal to the maximum diameter value found. The first element is the frequence of the diameter values = 1, the second element is the frequence of diameter values = 2, etc. If \code{cumulative} is True, the cumulative density is returned.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
    \code{\link[igraph]{diameter}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.diameters(g)
frag.diameters(g, cumulative=TRUE)

}

\keyword{graphs}
