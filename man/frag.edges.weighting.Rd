\name{frag.edges.weighting}
\alias{frag.edges.weighting}

\title{Weighting of an archeofrag graph}
\description{Weighting of the edges of a archeofrag fragmentation graph.}
\usage{frag.edges.weighting(graph, layer.attr)}

\arguments{
  \item{graph}{An undirected \code{igraph} object.} 
  \item{layer.attr}{Character. The name of the vertice attribute giving the layer of each fragment.}
}
\details{
Considering two layers, internal (within a layer) and external relations (between the two layers) are distinguished.
The weights of their edges is computed in two different ways.
Three subgraphs are first generated, one for each layer and one only for the external relations.

The weights of intra-layer edges (E) is equal to the sum of the degrees (d) of the vertices (i and j) they connect: 

    W_intra(E_ij)= di + dj  

For inter-layers edges, the same calculation is done but with a modifier to account for the balance of information available for each layer:  

    W_inter(E_ij)= (di + dj) * (1 + ( 1 / (1 +  sum(W_intra 1)/sum(W_intra 2)) ) )

with \code{W_intra 1} the internal weights within layer 1 and \code{W_intra 2} the internal weights within layer 2.

An error message is displayed if the vertices "layer" attribute has more than two layers.
}
\value{
The input graph, with a new "weight" edge attribute.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.edges.weighting(g , "layer")
}
 
\keyword{graphs}
