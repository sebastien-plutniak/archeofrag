\name{frag.get.parameters}
\alias{frag.get.parameters}

\title{Returns a series of descriptive statistics for a fragmentation graph}
\description{Returns a series of descriptive statistics for a fragmentation graph.}
\usage{frag.get.parameters(graph, layer.attr, verbose=TRUE)}

\arguments{
  \item{graph}{An \code{igraph} undirected graph.}
  \item{layer.attr}{Character. The name of the vertices attribute giving the spatial unit of each fragment (e.g. layer).}
  \item{verbose}{Logical. Whether to print or not warning messages.}
}
\details{
This function is a convenient function to get general information about a fragmentation graph. It is particularly useful for setting the parameters of the \code{frag.simul.process} function.
It returns:
\itemize{
  \item n.components: number of objects (i.e. connected components)
  \item vertices: number of fragments
  \item edges: number of connection relationships between fragments
  \item balance: estimated fragments balance, i.e. proportion of fragments in the first spatial unit (see details)
  \item components balance: estimated objects balance, i.e. proportion of objects in the first spatial unit (see details)
  \item disturbance: estimated disturbance, i.e. proportion of fragments which moved between spatial units (see details)
  \item aggreg.factor: aggregation factor, diversity of the components' edge count (see details)
  \item planar: whether the graph is planar or not
  \item edge.weights.sum: sum of the edge weights
  \item edge.weights.median: median of the edge weights
  \item edge.weights.median.abs.dev.: median absolute deviation of the edge weights
}

The (fragments) \bold{balance} and \bold{components balance} are calculated from the subgraph including only the fragments connected to fragments from the same spatial unit. This is to approximate these proportions of deposited materials regardless the effects of posterior disturbances. Results are given about the spatial unit whose label comes first alphanumerically.  

When studying a pair of spatial units, \bold{disturbance} is defined as the proportion of fragments which moved from their initial spatial unit to the other spatial unit. This definition is used in the \code{frag.process.simul} function to generate random fragmentation graphs.
However, \code{frag.get.parameters} uses a different definition since its aim is different: considering a fragmentation graph representing the result of unknown post-depositional processes, \code{frag.get.parameters} returns an estimation of the disturbance which might happened. This estimation is computed from a subgraph including only the components with fragments from the two spatial units: disturbance is get from the number of fragments belonging to the less represented spatial unit in this subgraph over the total number of fragments in the subgraph.

The \bold{aggregation factor} reflects the diversity of the components' edge count. The factor is calculated by: \deqn{1 - 1/(1 + sd(components\ edge\ count))}

The optional RBGL package is required to determine the planarity of the graph. If it is not installed, the \code{planar} argument is set to FALSE by default.
}
\value{
A list of parameters values (n.components, vertices, edges, balance, components.balance, disturbance, aggreg.factor, planar, edge.weights.sum, edge.weights.median, edge.weights.median.abs.dev.).
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
		 \link[archeofrag]{frag.get.layers.pair}, 
		 \link[archeofrag]{frag.simul.process}, 
		 \link[stats]{sd}, 
		 \link[stats]{median},
		 \link[stats]{mad}, 
		 \code{\link[RBGL]{boyerMyrvoldPlanarityTest}}
		 }

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=0.1)
frag.get.parameters(g, "layer")
}
\keyword{summary}
