\name{frag.edges.weighting}
\alias{frag.edges.weighting}

\title{Weighting of an archeofrag graph}
\description{Weighting of the edges of an archeofrag fragmentation graph.}
\usage{frag.edges.weighting(graph, layer.attr, morphometry, x, y, z)}

\arguments{
  \item{graph}{An undirected \code{igraph} object.} 
  \item{layer.attr}{Character. The name of the vertex attribute with the layer of the fragments.}
  \item{morphometry}{Character. Optional, the name of the vertex attribute with the morphometric value of the fragments.}
  \item{x}{Character. Optional, the name of the vertex attribute with the "x" coordinate of the fragments.}
  \item{y}{Character. Optional, the name of the vertex attribute with the "y" coordinate of the fragments.}
  \item{z}{Character. Optional, the name of the vertex attribute with the "z" coordinate of the fragments.}
}
\details{
In the framework of the TSAR method, this function weights the edges of a fragmentation graph, before performing an analysis.
The weights are computed from the topological properties  of the connection network and can be modified using  the morphometric properties of the fragments and/or the spatial distance between them. In summary, three different parameters can be used:

  1. topology, the basic weighting method;
  2. morphometry, the length, length by width, surface, volume, etc.
  3. spatial distance, expressed using the metric (or other) system, or an ad hoc relative system.

The function must be applied to a fragmentation graph with two layers.
Internal connection relationships (within a layer) and external relationships (between the two layers) are distinguished, and their respective edge weights are computed in different ways.
Three subgraphs are first generated, one for each layer and only one for the external relations.

The weight of an intra-layer edge (E) is equal to the sum of the degrees (d) of the vertices (i and j) it connects: 

    W_intra(E_ij)= di + dj  

For an inter-layer edge, the same calculation is made but with a modifier to account for the balance of information available for each layer:  

    W_inter(E_ij)= (di + dj) * (3 - (2 / (1 +  (trans_i+trans_j)/2)) * (1 - 1/(sqrt(V_sub + E_sub)))^2

with \code{trans_i} and \code{trans_i} the local transitivity of the vertices i and j, and \code{sqrt(V_sub + E_sub)} the square root of the sum of the vertices count and edge count of the sub-graph.

If the \code{morphometry} and/or coordinates (\code{x}, \code{y}, \code{z}) parameters are provided, the previous formula is modified using a factor which is computed as:

   f = 1 - sqrt( ((size_i+size_j)/max(sizes)) * sqrt((size_i/size_j)/max(prop)) * (distance_ij/max(distances)) ) 

with \code{size_i} the morphometric value of the smaller fragment, \code{max(sizes)} the maximum sum of morphometric values observed for the pairs of connected fragments in the dataset under study; \code{max(prop)} the maximum proportion between the size values of connected fragments observed in the dataset under study; distance ij the spatial distance between fragments i and j; \code{max(distances)} the maximum euclidean distance observed for the pairs of connected fragments in the dataset under study.
Results of the morphometric-spatial factor range in ]0,1].

Error messages are displayed if the vertex "layer" attribute has more than two layers, and a warning is displayed if one or more of the values for the x, y, z parameters do not exist in the input graph.
}
\value{
The graph, with an additional "weight" edge attribute and, if the distance has been computed, a "distance" edge attribute.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\seealso{
    \code{\link[igraph]{transitivity}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.edges.weighting(g , "layer")
# with morphometric and spatial parameters:
V(g)$morpho <- sample(1:20, 50, replace=TRUE)
V(g)$x <- sample(1:100, 50, replace=TRUE)
V(g)$y <- sample(1:100, 50, replace=TRUE)
V(g)$z <- sample(1:100, 50, replace=TRUE)
frag.edges.weighting(g , "layer", "morpho", "x", "y", "z")
}
 
\keyword{build data}
