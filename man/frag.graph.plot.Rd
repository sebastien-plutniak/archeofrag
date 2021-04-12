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
This function is a wrapper for the \code{plot.igraph} method for igraph objects. The layout is computed using the fruchterman-reingold algorithm, with some change in function of the value of the "frag_type" graph attribute.
For graphs including similarity relations, \code{igraph}' \code{component_wise} layout modifier is applied.
For graphs with connection and similarity relations, the edges for connection relations are colored in green.
For graphs with connection relations only and two layers, the nodes from the two layers are located based on their layer in the upper and the lower part of the plot.

}
\value{
    Returns \code{NULL} and plot the graph.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
  \link[igraph]{plot.igraph}, \link[igraph]{component_wise}, \link[igraph]{layout_with_fr}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.graph.plot(g, "layer")
}
 
\keyword{summary}
