\name{frag.layers.admixture}
\alias{frag.layers.admixture}

\title{Admixture measure of two stratigraphic layers}
\description{Measure the admixture of two layers.}
\usage{frag.layers.admixture(graph, layer.attr)}

\arguments{
  \item{graph}{An undirected \code{igraph} object.} 
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of each fragment.}
}
\details{
The admixture value is given by: 

    \code{1 - cohesion(layer 1) - cohesion(layer 2)}

Result ranges between [0;1] with 0 for unmixed layers and values towards 1 for highly mixed layers.
This function must be used after weighting the edges with \code{frag.edges.weighting}.
An error message is displayed if the vertex "layer" attribute has more than two layers.
}
\value{
The admixture value of the pair of layers.
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
