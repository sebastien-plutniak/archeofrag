\name{frag.layers.cohesion}
\alias{frag.layers.cohesion}

\title{Cohesion measure of layers}
\description{Returns the cohesion value of two archaeological spatial units (layers or any other type of spatial units).}
\usage{frag.layers.cohesion(graph, layer.attr, morphometry, x, y, z, verbose=TRUE)}

\arguments{
  \item{graph}{An undirected \code{igraph} object.} 
  \item{layer.attr}{Character. The name of the vertices attribute giving the spatial unit of the fragments.}
  \item{morphometry}{Character. Optional, to pass to the `frag.edges.weighting` function: name of the vertex attribute with the morphometric value of the fragments.}
  \item{x}{Character. Optional, to pass to the `frag.edges.weighting` function: name of the vertex attribute with the "x" coordinate of the fragments.}
  \item{y}{Character. Optional, to pass to the `frag.edges.weighting` function: name of the vertex attribute with the "y" coordinate of the fragments.}
  \item{z}{Character. Optional, to pass to the `frag.edges.weighting` function:  name of the vertex attribute with the "z" coordinate of the fragments.}
  \item{verbose}{Logical. Whether to print or not warning messages.}
}
\details{
Note that this function  must be used after weighting the edges with \code{frag.edges.weighting}.

The cohesion value of a spatial unit  is computed as: 

	\deqn{\frac{V_{unit_i} + W_{unit_i}}{ \sum_{j=1}^{2} V_{unit_j} + W_{unit_j}} }
	
with V the number of vertices in the unit and W the sum of the edge weights within the unit.

The measure takes into account the balance between the information about each spatial unit.
Results range in [0;1], with 0 for two spatial units with only inter-spatial units connection relationships, and 1 if there are not inter-spatial units relationships and a  significant imbalance of information on the two spatial units.

The basic use of this function is intended for a graph with two spatial units, whose edges have been previously weighted using the \code{frag.edges.weighting} function (an error message is displayed if the vertice attribute "layer" contains less than two spatial units, and if the graph does not have an edge attribute "weight").

However, this function can also be used for a graph with more than two spatial units. In this case, a subgraph is generated for each pair of spatial units (using the \code{frag.get.layers.pair} function), the \code{frag.edges.weighting} function is applied to weight their edges (a warning message is displayed), and the cohesion is computed.
}
\value{
If the graph has only two spatial units, the function returns a numeric vector with a cohesion value ([0;1]) for each spatial units (sorted in alphanumerical order).
If the graph has more than two spatial units, the function returns a matrix with cohesion values for each pair of spatial units.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
\code{\link[archeofrag]{frag.edges.weighting}},
\code{\link[archeofrag]{frag.get.layers.pair}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.1)
frag.layers.cohesion(g, layer.attr="layer")
}
 
\keyword{measurement}
