\name{frag.get.layers.pair}
\alias{frag.get.layers.pair}
\title{
Extracts the subgraph corresponding to a pair of stratigraphic layers.
}
\description{
Extracts the subraph corresponding to a pair of stratigraphic layers (or any other type of archaeological spatial unit).
}
\usage{
frag.get.layers.pair(graph, layer.attr, sel.layers, size.mini=2,
                     mixed.components.only=FALSE)
}

\arguments{
  \item{graph}{An \code{igraph} object.}
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of each fragment.}
  \item{sel.layers}{A numeric vector of length 2 with the name of the stratigraphic layer selected for extraction.}
  \item{size.mini}{A minimal number of vertices for the components to include in the resulting graph.}
  \item{mixed.components.only}{Logical. If TRUE, only the components with fragments from the two selected layers are returned. If FALSE, all the components of the two layers are extracted.}
}
\details{
The default setting of the \code{mixed.components.only} argument is FALSE, for convenience for other measurements.
}
\value{
An undirected graph object.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
igraph::V(g)$layers <- c(rep("layer1", 20), rep("layer2", 20), rep("layer3", 10))

frag.get.layers.pair(g, "layers", sel.layers=c("layer2","layer3"),
                     size.mini=2, mixed.components.only=FALSE)
frag.get.layers.pair(g, "layers", sel.layers=c("layer2","layer3"),  
                     size.mini=2, mixed.components.only=TRUE)
}
\keyword{extraction}
