\name{frag.simul.process}
\alias{frag.simul.process}
\title{
Simulate the fragmentation of archaeological objects scattered in two spatial units
}
\description{
Simulate the fragmentation of archaeological objects scattered in two spatial units.
}
\usage{
frag.simul.process(initial.layers=2, n.components=NULL, vertices=Inf,
                   edges=Inf, balance=.5, components.balance=.5,
                   disturbance=0, aggreg.factor=0, planar=FALSE,
                   asymmetric.transport.from=NULL, 
                   from.observed.graph=NULL, observed.layer.attr=NULL, 
                   verbose=TRUE)
}

\arguments{
  \item{initial.layers}{Integer (1 or 2). Number of hypothetical spatial units (e.g. layers) to use as initial condition.}
  \item{n.components}{Integer. Innitial number of objects (connected components).}
  \item{vertices}{Integer. Final number of fragments (vertices).}
  \item{edges}{Integer. Final number of connection relationships between fragments (edges).}
  \item{balance}{Numeric ]0;1[. Proportion of fragments to generate in the first spatial unit before applying disturbance.}
  \item{components.balance}{Numeric ]0;1[. Proportion of objects (connected components) in the first spatial unit before applying fragmentation (only used when initial.layers=2).}
  \item{disturbance}{Numeric [0;1]. Final proportion of fragments moved from a spatial unit to the other.}
  \item{aggreg.factor}{Numeric [0;1]. Higher values increase the likelihood that the biggest components are selected when adding fragments or connections (see details).}
  \item{planar}{Logical. If TRUE, generates a planar graph (if FALSE, the graph can be planar or not).}
  \item{asymmetric.transport.from}{Numeric or character value in "0", "1", and "2". Determine the direction of fragment transport when generating disturbance. Null or "0" value is for bidirectional transport,  "1" and "2" are for asymmetric transport from the first and second spatial units, respectively.}
  \item{from.observed.graph}{igraph object. If not NULL, the parameters observed in this fragmentation graph are used instead of the previous parameters. See details.}
  \item{observed.layer.attr}{character. Required if the \code{from.observed.graph} option is used. Name of the spatial unit attribute in the observed graph.}
  \item{verbose}{Logical. Whether to print or not warning messages.}
}
\details{
This function simulates the fragmentation of archeological objects within and between two adjacent stratigraphic layers.
Fragments are represented by vertices and the "connection" relationships ("refittings") between them are represented by edges.
All fragments have at least one relation ("single" fragments are not generated).

Some parameters are optional or depend on other parameters (messages are displayed accordingly). 
Namely, setting \code{initial.layers}=1 enables to constraint the graph with the number of vertices only, the number of edges only, or both. 
With \code{initial.layers}=2, the \code{components.balance} can be used, and the \code{edges} parameter is not supported (only the \code{vertices} parameter can be used).

The \code{components.balance} parameter determines the proportion of components (i.e. objects) in the first spatial unit before the application of the fragmentation process;
the \code{balance} parameter determines the proportion of fragments in the first spatial unit before the application of the disturbance process.
The \code{disturbance} parameter determines the proportion of fragments to move from one spatial unit to another. Consequently, it generates inter-spatial units relationships. Highest admixture values are generated with disturbance=0.5; disturbance values > 0.5 revert the fragment balance between the two spatial units. 
The \code{aggreg.factor} contraints the selection of the object (set of fragments, i.e. connected components) to be fragmented at each iteration of the simulation: with \code{aggreg.factor=0}, the probability is equal for all objects; with \code{aggreg.factor=0.01}  the probability to select the 1st largest component is 0.99 and the smallest component is 1/4; with \code{aggreg.factor=1} the probability to select the 1st largest component is 1/2, the second largest component is 1/3, the third largest is 1/4, etc.
If \code{asymmetric.transport.from} is set to 1 or 2, the disturbance process is only applied to the fragments from spatial unit 1 or spatial unit 2, respectively.

If a graph is given to the \code{from.observed.graph} parameter, the properties of this graph are internally retrieved with the \code{frag.get.parameters} function (including:  the number of components, number of vertices, balance, the components.balance, the disturbance, the aggregation factor, and whether the graph is planar or not; note that the number of edges is not included as a parameter). If some other parameters of the \code{frag.simul.process} function are set, the values retrieved from the observed graph are used in replacement.
The \code{frag.edges.weighting} is internally applied to weight the graph edges.

Setting the \code{planar} argument to TRUE constraints the graph to be planar (if this parameter is FALSE, the graph can be planar or not). 
Note that using the \code{planar} argument requires to install the optional RBGL package and that the simulator is faster with \code{initial.layers}=2 and  \code{planar}=FALSE.
}

\value{
An igraph object with a "frag_type" graph attribute (with the value "cr", for "connection relationship") and three vertices attributes: "name" (vertices identifiers), "layer" (with the values "1" and "2"), and "object.id" (component identifiers).

}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\seealso{
    \code{\link[archeofrag]{frag.get.parameters}},
    \code{\link[archeofrag]{frag.edges.weighting}},
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
                            planar=FALSE)
plot(g, vertex.color=factor(igraph::V(g)$layer), 
     vertex.size=4, vertex.label=NA)

}
\keyword{simulation}
