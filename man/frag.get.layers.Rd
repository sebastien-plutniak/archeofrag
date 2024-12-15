\name{frag.get.layers}
\alias{frag.get.layers}
\title{
Extracts the subgraph of each selected archaeological spatial unit.
}
\description{
Extracts the subgraph of each selected archaeological spatial unit (stratigraphic layer or any other type of spatial unit).
}
\usage{
frag.get.layers(graph, layer.attr, sel.layers)
}
\arguments{
  \item{graph}{An undirected \code{igraph} object.}
  \item{layer.attr}{Character. The name of the vertices attribute giving the spatial unit of each fragment.}
  \item{sel.layers}{Character. The identifier(s) of the stratigraphic spatial units to retrieve.}
}
\details{
This function is only a convenient function to extract the subgraphs of selected  archaeological spatial unit (stratigraphic layer or any other type of spatial unit).
A graph is created for each layer in the vertex attribute given by the \code{layer.attr} argument.
}
\value{
A list with a graph for each selected stratigraphic layer.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance = .15)
igraph::V(g)$layers <- c(rep("layer1", 20), rep("layer2", 20), rep("layer3", 10))
frag.get.layers(g, layer.attr="layers", sel.layers=c("layer1", "layer2"))
}
\keyword{extraction}
