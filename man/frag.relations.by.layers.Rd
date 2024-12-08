\name{frag.relations.by.layers}
\alias{frag.relations.by.layers}
\title{
Summary of the connection relationships between fragments within and between spatial units.
}
\description{
Return a matrix with the number of relationships within and between spatial units (e.g., layers).
}
\usage{
frag.relations.by.layers(graph, layer.attr)
}
\arguments{
  \item{graph}{An \code{igraph} object.}
  \item{layer.attr}{Character. The name of the vertices attribute giving the spatial unit of each fragment.}
}
\details{
This function is a useful method to summarise the distribution of connection relationships within and between spatial units (e.g., layers).
}
\value{
A square matrix with the number of connection relationships within and between spatial units. 
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
frag.relations.by.layers(g, "layer")
}
\keyword{summary}
