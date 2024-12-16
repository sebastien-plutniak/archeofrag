\name{frag.graph.reduce}
\alias{frag.graph.reduce}

\title{Reduce the size of a fragmentation graph}
\description{Remove fragments from a fragmentation graph}

\usage{frag.graph.reduce(graph=NULL, n.frag.to.remove=NULL, conserve.objects.nr=FALSE)}

\arguments{
  \item{graph}{An \code{igraph} object.} 
  \item{n.frag.to.remove}{Integer.  Number of fragments (i.e. vertices) to remove.}
  \item{conserve.objects.nr}{Logical. If TRUE, preserve the number of objects (i.e. connected components) in the graph.}
  
}
\details{
This function reduces the number of fragments in a fragmentation graph, while controlling that no singleton are created, and (optionally) that the output graph and the input graph have the same number of connected components. Note that if 'conserve.objects.nr' is TRUE then the reduction process can stop before reaching the number of fragments to remove (when the graph only contains pairs of fragments).
}
\value{
A fragmentation graph (igraph object) with less fragments (vertices).
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\examples{
g <- frag.simul.process(n.components=15, vertices=50, disturbance = .15) 
igraph::gorder(g)
igraph::components(g)$no
# reduce the number of framents and conserve the number of connected components:
g1 <- frag.graph.reduce(g,  n.frag.to.remove = 40, conserve.objects.nr = TRUE)
igraph::gorder(g1)
igraph::components(g1)$no
# reduce the number of framents and do not conserve the number of connected components:
g2 <- frag.graph.reduce(g,  n.frag.to.remove = 40, conserve.objects.nr = FALSE)
igraph::gorder(g2)
igraph::components(g2)$no
}
 
\keyword{simulation}
