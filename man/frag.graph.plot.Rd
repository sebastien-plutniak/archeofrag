\name{frag.graph.plot}
\alias{frag.graph.plot}

\title{Plot a fragmentation graph}
\description{A function to plot the graph made by the \code{archeofrag} package. }
\usage{frag.graph.plot(graph, layer.attr, ...)}

\arguments{
  \item{graph}{An \code{igraph} undirected object with a "frag_type" attribute.} 
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of each fragment.}
  \item{...}{Optional arguments sent to \code{plot.igraph}.}
}
\details{
    This function is a wrapper for the \code{plot.igraph} method for igraph objects, using the fruchterman-reingold algorithm. The layout of the graph is modified in function of the value of the "frag_type" graph attribute: for "connection and similarity relations" value, \code{igraph}' \code{component_wise} layout modifier is used.
}
\value{
    Returns \code{NULL} and plot the graph.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
  \link[igraph]{plot.igraph}, \link[igraph]{component_wise}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.graph.plot(g, "layer")
}
 
\keyword{graphs}
