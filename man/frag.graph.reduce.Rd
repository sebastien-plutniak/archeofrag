\name{frag.graph.reduce}
\alias{frag.graph.reduce}

\title{Reduce the size of a fragmentation graph}
\description{Remove fragments from a fragmentation graph}

\usage{frag.graph.reduce(graph=NULL, n.frag.to.remove=NULL, conserve.objects.nr=FALSE,
       conserve.fragments.balance=FALSE, conserve.inter.units.connection=FALSE,
       verbose=FALSE)}

\arguments{
  \item{graph}{An \code{igraph} object.} 
  \item{n.frag.to.remove}{Integer. Number of fragments (i.e. vertices) to remove.}
  \item{conserve.objects.nr}{Logical. If TRUE, preserve the number of objects (i.e. connected components) in the graph.}
  \item{conserve.fragments.balance}{Logical. If TRUE, try to preserve the proportion of fragments in the first and second spatial units ('balance').}
  \item{conserve.inter.units.connection}{Logical. If TRUE, preserve the proportion of connection relationships between fragments located in different spatial units (see details).}
  \item{verbose}{Logical. Whether to print or not warning messages.}
  
}
\details{
This function reduces the number of fragments in a fragmentation graph, while controlling that no singleton are created, and (optionally) that the output graph and the input graph have the same number of connected components. Note that if 'conserve.objects.nr' is TRUE then the reduction process might stop before reaching the number of fragments to remove (when the graph only contains pairs of fragments). 
Similarly, if 'conserve.fragments.balance' is TRUE, then the result is as close as possible from the initial proportion of fragments (i.e. 'frag.get.parameters()' 'balance' output value). 
Finally, if 'conserve.inter.units.connection' is TRUE, the proportion of connection relationships (i.e. graph edges) between fragments from different spatial units to conserve is equal to the proportion of fragments (i.e. vertices) to preserve.
}
\value{
A fragmentation graph (igraph object) with less fragments (vertices).
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\seealso{
    \code{\link[archeofrag]{frag.get.parameters}}
    \code{\link[archeofrag]{frag.get.layers.pair}}
}

\examples{
g <- frag.simul.process(n.components=15, vertices=50, disturbance=.15) 
igraph::gorder(g)
igraph::components(g)$no
# reduce the number of fragments and conserve the number of connected components:
g1 <- frag.graph.reduce(g, n.frag.to.remove = 40, conserve.objects.nr = TRUE)
igraph::gorder(g1)
igraph::components(g1)$no
# reduce the number of fragments and do not conserve the number of connected components:
g2 <- frag.graph.reduce(g, n.frag.to.remove = 40, conserve.objects.nr = FALSE)
igraph::gorder(g2)
igraph::components(g2)$no
}
 
\keyword{simulation}
