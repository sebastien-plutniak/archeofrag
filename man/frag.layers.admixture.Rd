\name{frag.layers.admixture}
\alias{frag.layers.admixture}

\title{Admixture of two stratigraphic layers}
\description{Evaluate how reliable is the distinction between two layers (or any other kind of archaeological spatial unit).}
\usage{frag.layers.admixture(graph, layer.attr)}

\arguments{
  \item{graph}{An undirected \code{igraph} object.} 
  \item{layer.attr}{Character. The name of the vertex attribute giving the layer of each fragment.}
}
\details{
Given two layers (or other spatial unit) containing connected fragments, this function evaluate how reliable is their distinction, using the formula:

    \code{1 - cohesion(layer 1) - cohesion(layer 2)}

So, in this method, the admixture of two layers is equal to the cohesion of an arbitrary third layers defined by the fragments and connection at the intersection of the two layers.
This function must be used after weighting the edges with \code{frag.edges.weighting}.
Results range in [0;1] with 0 for completely independent layers and values towards 1 for mixed layers.

An error message is displayed if the vertex "layer" attribute has more than two layers.
}
\value{
A numerical value reflecting the admixture of the two layers.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
\code{\link[archeofrag]{frag.edges.weighting}},
\code{\link[archeofrag]{frag.simul.process}},
\code{\link[RBGL]{boyerMyrvoldPlanarityTest}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance = .15)
g <- frag.edges.weighting(g, layer.attr="layer")
frag.layers.admixture(g, "layer")
}
 
\keyword{graphs}
