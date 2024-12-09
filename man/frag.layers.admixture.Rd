\name{frag.layers.admixture}
\alias{frag.layers.admixture}

\title{Admixture of two stratigraphic layers}

\description{Evaluate how reliable the distinction is between the two layers (or any other type of archaeological spatial units).}
\usage{frag.layers.admixture(graph, layer.attr, morphometry, x, y, z, verbose=TRUE)}

\arguments{
  \item{graph}{An undirected \code{igraph} object.} 
  \item{layer.attr}{Character. The name of the vertex attribute giving the layer of each fragment.}
  \item{morphometry}{Character. Optional, to pass to the `frag.edges.weighting` function: name of the vertex attribute with the morphometric value of the fragments.}
  \item{x}{Character. Optional, to pass to the `frag.edges.weighting` function: name of the vertex attribute with the "x" coordinate of the fragments.}
  \item{y}{Character. Optional, to pass to the `frag.edges.weighting` function: name of the vertex attribute with the "y" coordinate of the fragments.}
  \item{z}{Character. Optional, to pass to the `frag.edges.weighting` function:  name of the vertex attribute with the "z" coordinate of the fragments.}  
  \item{verbose}{Logical. Whether to print or not warning messages.}
}
\details{
This function returns a value reflecting the robustness of the distinction between two layers (or any other relevant archaeological spatial unit).
The admixture value is computed as:

    \code{1 - cohesion(layer 1) - cohesion(layer 2)}

The admixture of two layers is equal to the cohesion of a virtual third layer, which is defined by the fragments and the connection relationships intersecting the two layers. 
Results range in [0;1] with 0 for two completely independent layers and values towards 1 as the robustness of the boundary between the two layers is lower.
As it appears, this function calls the \code{frag.layers.cohesion} function.

The basic use of this function is intended for a graph with two layers, whose edges have been previously weighted using the \code{frag.edges.weighting} function (an error message is displayed if the vertice attribute "layer" contains less than two layers, and if the graph does not have an edge attribute "weight").

However, the function can also be used for a graph with more than two layers. In this case, a subgraph is generated for each pair of layers (using the \code{frag.get.layers.pair} function), the \code{frag.edges.weighting} function is applied to weight their edges (a warning message is displayed), and the admixture is computed.

An error message is displayed if the vertex "layer" attribute has less than two layers.
}
\value{
A numeric vector with the admixture of each pair of layers.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
\code{\link[archeofrag]{frag.edges.weighting}},
\code{\link[archeofrag]{frag.layers.cohesion}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance = .15)
g <- frag.edges.weighting(g, layer.attr="layer")
frag.layers.admixture(g, "layer")
}
 
\keyword{measurement}
