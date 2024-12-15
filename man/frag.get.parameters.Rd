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
  \item{number of components}{}
  \item{number of fragments}{}
  \item{number of edges}{}
  \item{balance}{proportion of fragments in the spatial unit with less fragments (see details)}
  \item{components balance}{proportion of components in the spatial unit with less components (see details)}
  \item{disturbance}{an estimation of disturbance (see details)}
  \item{aggregation factor (see details)}
  \item{planar}{whether the graph is planar or not}
}

The balance and components balance are calculated from the subgraph including only fragments connected to fragments from the same spatial unit. This is to approximate these proportions of deposited materials regardless the effects of posterior disturbances.

The disturbance is estimated using the subgraph including only components with fragments from the two spatial units: this value is computed as the number of fragments belonging to the less represented spatial unit in each component over the total number of fragments in the considered subgraph.

The aggregation factor reflects the diversity of the components' edge count. The factor is calculated by: \deqn{1 - 1/(1 + sd(edge count by components))}.

The optional RBGL package is required to determine the planarity of the graph. If it is not installed, the \code{planar} argument is set to FALSE by default.
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
