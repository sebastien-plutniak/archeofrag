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
Considerng two layers, internal (inside a layer) and external relations (between the two layers) are distinguished.
The weights of their edges is computed in different ways.
Three subgraphs are first generated, one for each layer and one only for the external relations.


Intra-layer edges (E) are given the sum of the degrees (d) of the vertices (i and j) they connect: 

    W_intra(E_ij)= di + dj  

For extra-layers, edges the same computation is done but with an alteration in order to take into account the balance between the set of fragments related to each layer:  

    W_extra(E_ij)= (di + dj) * TODO

with Gmin the minimal number of fragments from the same layer in the given connected set of fragments, and Gmax the maximal number of fragments from the same layer in the given connected set of fragments.  

An error message is displayed if the vertice "layer" attribute has more than two layers.
}
\value{
The input graph, with a new "weight" edge attribute.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
g <- frag.edges.weighting(g , "layer")
E(g)$weight
}
 
\keyword{graphs}
