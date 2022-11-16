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
This function is a convenient function to obtain general information about a fragmentation graph. It is particularly useful for setting the parameters of the \code{frag.simul.process} function.
It returns the number of components, vertices, and edges, the balance (proportion of fragments in the smaller layer), components balance (proportion of components in the poorest layer), an estimation of the disturbance, the aggregation factor, and whether the graph is planar or not.

The disturbance is estimated from the subset of components with fragments from the two layers: it is computed as the number of fragments belonging to the less represented layer in each component over the total number of fragments in this subset of components.
The aggregation factor reflects the diversity of the components' edge counts. The factor is calculated by: 1 - 1/(1 + sd(edge counts of the components)).
The optional RBGL package is required to determine the planarity of the graph. If it is not installed, the `planar` value is set to FALSE by default.
}
\value{
A list of parameters values (n.components, vertices, edges, balance, components.balance, disturbance, aggreg.factor, planar).
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
		 \link[archeofrag]{frag.get.layers.pair}, 
		 \link[archeofrag]{frag.simul.process}, 
		 \link[stats]{sd}, 
		 \code{\link[RBGL]{boyerMyrvoldPlanarityTest}}
		 }

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=0.1)
frag.get.parameters(g, "layer")
}
\keyword{summary}
