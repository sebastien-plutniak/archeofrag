\name{frag.simul.process}
\alias{frag.simul.process}
\title{
Simulate the fragmentation of archaeological objects scattered in two stratigraphic layers
}
\description{
Simulate the fragmentation of archaeological objects scattered in two stratigraphic layers (or any other kind of spatial unit).
}
\usage{
frag.simul.process(initial.layers=2, n.components, vertices=Inf,
                   edges=Inf, balance=.5, components.balance=.5,
                   disturbance=0, aggreg.factor=0, planar=TRUE,
                   asymmetric.transport.from=NULL, 
                   from.observed.graph=NULL, observed.layer.attr=NULL)
}

\arguments{
  \item{initial.layers}{Integer (1 or 2). Number of hypothetical stratigraphic layers to use as initial condition.}
  \item{n.components}{Integer. Number of objects to fragment (connected components).}
  \item{vertices}{Integer. Number of fragments (vertices).}
  \item{edges}{Integer. Number of connection relationships between fragments (edges).}
  \item{balance}{Numeric ]0;1[. Proportion of fragments in the first layer.}
  \item{components.balance}{Numeric ]0;1[. Proportion of components in the first layer (only used when initial.layers=2).}
  \item{disturbance}{Numeric [0;1]. Proportion of fragments to randomly move from one layer to another.}
  \item{aggreg.factor}{Numeric [0;1]. Higher values increase the likelihood that the biggest components are selected when adding fragments or connections.}
  \item{planar}{Logical. If TRUE, generates a planar graph.}
  \item{asymmetric.transport.from}{Numeric or character value in "1" or "2". If not NULL, the disturbance process will be applied only to the fragments from this layer.}
  \item{from.observed.graph}{igraph object. If not NULL, the parameters observed in this fragmentation graph are used instead of the previous parameters. See details.}
  \item{observed.layer.attr}{character. Required if the \code{from.observed.graph} option is used. Name of the layer attribute in the observed graph.}
}
\details{
This function simulates the fragmentation of archeological objects within and between two adjacent stratigraphic layers.
Fragments are represented by vertices and the "connection" relationships ("refittings") between them are represented by edges.

Some parameters are optional or depend on other parameters (messages are displayed accordingly). Namely, if two \code{initial.layers} are set, then only one of the \code{vertices} and \code{edges} parameters can be used. Using only one layer as the initial condition enables to constraint the graph with the number of vertices only, the number of edges only, or both. 

The \code{disturbance} determines the proportion of fragments to "move" from one layer to another. Consequently, it generates inter-layer relationships.
Note that the \code{balance} parameter determines the proportion of fragments in the first layer before the application of the disturbance process.

If a graph is given to the \code{from.observed.graph} parameter, the properties of this graph are internally retrieved with the \code{frag.get.parameters} function (including:  the number of components, number of vertices, balance, the components.balance, the disturbance, the aggregation factor, and whether the graph is planar or not; note that the number of edges is not included as a parameter). If some other parameters of the \code{frag.simul.process} function are set, the values retrieved from the observed graph are used in replacement.
The \code{frag.edges.weighting} is internally applied to weight the graph edges.

Note that the simulator is faster with 2 \code{initial.layers} and when not using the planarity constraint.
}
\value{
An igraph object with a "frag_type" graph attribute (with the value "cr", for "connection relationship") and three vertices attributes: "name" (vertices identifiers), "layer" (with the values "1" and "2"), and "object.id" (component identifiers).

}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\seealso{
    \code{\link[RBGL]{boyerMyrvoldPlanarityTest}},
    \code{\link[archeofrag]{frag.get.parameters}},
    \code{\link[archeofrag]{frag.edges.weighting}}
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
\keyword{simulation}
