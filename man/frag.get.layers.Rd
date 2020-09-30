\name{frag.get.layers}
\alias{frag.get.layers}
\title{
Returns the subgraph of each selected stratigraphic layers.
}
\description{
Returns the subgraph of each selected stratigraphic layers.
}
\usage{
frag.get.layers(graph, layer.attr, sel.layers)
}
\arguments{
  \item{graph}{An undirected \code{igraph} object.}
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of each fragment.}
  \item{sel.layers}{A numeric vector with the stratigraphic layers to be extracted.}
}
\details{
This function is only a convenient function to extract the subgraphs of selected stratigraphic layers. 
A graph is created for each layer in the vertex attribute given by the \code{layer.attr} argument.
}
\value{
A list with a graph for each selected stratigraphic layers.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance = .15)
V(g)$layers <- c(rep("layer1", 20), rep("layer2", 20), rep("layer3", 10))
frag.get.layers(g, layer.attr="layers", sel.layers=c("layer1", "layer2"))
}
\keyword{graphs}
