\name{frag.simul.process}
\alias{frag.simul.process}
\title{
Simulate the fragmentation of archaeological objects scattered in two stratigraphic layers.
}
\description{
Simulate the fragmentation of archaeological objects scattered in two stratigraphic layers.
}
\usage{
frag.simul.process(initial.layers=2, n.components, vertices=Inf, edges=Inf, balance=.5, components.balance=NULL, disturbance=0, aggreg.factor=0, planar=TRUE)
}

\arguments{
  \item{initial.layers}{Integer (1 or 2). Number of hypothetical stratigraphic layers to use as initial condition.}
  \item{n.components}{Integer. Number of objets to fragment (connected components).}
  \item{vertices}{Integer. Number of fragments (vertices).}
  \item{edges}{Integer. Number of connection relationships between fragments (edges).}
  \item{balance}{Numeric ]0;1[. Proportion of fragments in the first layer.}
  \item{components.balance}{Numeric ]0;1[. Proportion of fragments in the first layer.}
  \item{disturbance}{Numeric [0;1]. Proportion of fragments to randomly move from a layer to the other.}
  \item{aggreg.factor}{Numeric [0;1]. Higher values increase the likelihood that the biggest components are selected when adding fragments or connections.}
  \item{planar}{Logical. If TRUE, generates a planar graph.}
}
\details{
This function simulates the fragmentation of archeological objects within and between two adjacent stratigraphic layers.
Fragments are represented by vertices and the "connection" relationships ("refittings") between them are represented by edges.

Some parameters are optional or depends on other parameters (messages are displayed accordingly). Namely, if two \code{initial.layers} are set, then only one of the \code{vertices} and \code{edges} parameters can be used. Using only one layer as initial condition enables to constraint the graph with both vertices and edges number. If no value is set for \code{components.balance} then the \code{balance} value is used instead (its default value is 0.5).

The \code{disturbance} determines the proportion of fragments to "move" from a layer to the other one. Consequently, it generates inter-layers relationships.

Note that the simulator is faster with 2 \code{initial.layers} and when not using the planarity constraint.
}
\value{
An igraph object with a "frag_type" graph attribute (with the value "cr", for "connection relationship") and three vertices attributes: "name" (vertices identifiers), "layer" (with "1" and "2" values), and "object.id" (components identifiers).

}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\seealso{
    \code{\link[RBGL]{boyerMyrvoldPlanarityTest}}
}

\examples{
frag.simul.process(n.components=20, vertices=50, disturbance=.15)

g <- frag.simul.process(initial.layers=1, 
                            n.components=20,
                            vertices=50,
                            edges=40,
                            balance=.5,
                            components.balance=.5,
                            disturbance=.1,
                            planar=TRUE)
plot(g, vertex.color=factor(V(g)$layer), 
     vertex.size=4, vertex.label=NA)

}
\keyword{graphs}
