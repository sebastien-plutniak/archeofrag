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
It returns the number of components, vertices, and edges, the balance (proportion of fragments in the smaller layer), components balance (proportion of components in the poorest layer), the disturbance, the aggregation factor, and if the graph is planar or not.
The aggregation factor reflects the diversity of the components edge count. The factor is calculated by: 1 - 1/(1 + sd(edge counts of the components)).
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
\keyword{summary}
