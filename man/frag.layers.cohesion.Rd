\name{frag.layers.cohesion}
\alias{frag.layers.cohesion}

\title{Cohesion measure for two layers}
\description{Returns a cohesion value for each stratigraphic layer. Must be used after weighting the edges with \code{frag.edges.weighting}.}
\usage{frag.layers.cohesion(graph, layer.attr)}

\arguments{
  \item{graph}{An undirected \code{igraph} object.} 
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of each fragment.}
}
\details{
This function returns a vector with a cohesion value for each layer. 

The cohesion value for a given layer is given by: 

    \code{sum(W_inLayer) / (sum(w_in) + sum(W_ex))}


with W_inLayer for the weights of the internal edges of the layer under consideration, W_in for the weights of all the internal edges,  and W_ex for the weights of the external edges. 

The measure integrates the balance between the information about each layer.
Results range in [0;1] with 0 for two layers only with inter-layers connection relationships, and 1 if there is no inter-layers relationships and important inequal information about the two layers.

An error message is displayed if the vertice "layer" attribute has more or less than two layers.
}
\value{
A numeric vector with the cohesion values ([0;1]) for each layer of the graph.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
\code{\link[archeofrag]{frag.edges.weighting}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.1)
g <- frag.edges.weighting(g, layer.attr="layer")
cohesion.values <- frag.layers.cohesion(g, layer.attr="layer")
}
 
\keyword{graphs}
