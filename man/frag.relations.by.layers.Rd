\name{frag.relations.by.layers}
\alias{frag.relations.by.layers}
\title{
Summary of the connection relationships between fragments within and between stratigraphic layers.
}
\description{
Return a matrix with the number of relations within and between stratigraphic layers (or any othe type of archaeological spatial units).
}
\usage{
frag.relations.by.layers(graph, layer.attr)
}
\arguments{
  \item{graph}{An \code{igraph} object.}
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of each fragment.}
}
\details{
This function is a useful method to summarise the distribution of relationships within and between stratigraphic layers (or any othe type of archaeological spatial units).
}
\value{
A symmetrical matrix with the number of relationships within and between the layers. 
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.relations.by.layers(g, "layer")
}
\keyword{summary}
