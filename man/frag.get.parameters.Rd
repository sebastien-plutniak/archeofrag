\name{frag.get.parameters}
\alias{frag.get.parameters}

\title{Returns a series of descriptive statistics for a fragmentation graph}
\description{Returns a series of descriptive statistics for a fragmentation graph.}
\usage{frag.get.parameters(graph, layer.attr)}

\arguments{
  \item{graph}{An \code{igraph} undirected graph.}
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of each fragment.}
}
\details{
This function is a convenient function to get general information about a fragmentation graph. It is particularly handy for setting the parameters of the \code{frag.simul.process} function.
It returns the number of components, vertices, and edges, the balance (proportion of fragments in the smaller layer), components balance (proportion of components in the poorest layer), the disturbance, and the aggregation factor.
The aggregation factor is determined by the formula: 1 - 1/(1 + sd(edge count of the components)).
}
\value{
A list with the values for the parameters.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{\link{frag.get.layers.pair}, \link{frag.simul.process}}

\examples{
g <- frag.simul.process(n.components=20, vertices=50)
frag.get.parameters(g, "layer")
}
\keyword{graphs}
