\name{frag.simul.summarise}
\alias{frag.simul.summarise}

\title{Summarise the comparison between an observed fragmentation graph and simulated graphs for two deposition hypotheses.}
\description{Compare the parameters measured on simulated graphs generated for two deposition hypotheses with Wilcoxon tests, and the corresponding values measured on an empirical graph, and returns a summary data frame.}
\usage{frag.simul.summarise(graph, layer.attr, res.h1, res.h2, cohesion1.attr="cohesion1",
                            cohesion2.attr="cohesion2", admixture.attr="admixture")}

\arguments{
  \item{graph}{An undirected \code{igraph} object. The graph to compare with simulated graphs.} 
  \item{layer.attr}{Character. The name of the vertices attribute giving the spatial unit of the fragments (layer or other).}
  \item{res.h1}{data frame. A data frame with the parameters observed on the simulated graphs for H1.}
  \item{res.h2}{data frame. A data frame with the parameters observed on the simulated graphs for H2.}
  \item{cohesion1.attr}{character. The name of the column in the data frames res.h1 and res.h2 with the cohesion values of the spatial unit 1.}
  \item{cohesion2.attr}{character. The name of the column in the data frames res.h1 and res.h2 with the cohesion values of the spatial unit 2.}
  \item{admixture.attr}{character. The name of the column in the data frames res.h1 and res.h2 with the admixture values.}
}
\details{
This function compares and summarises the numerical values measured on the series of graphs generated for the two deposition hypotheses (H1: one initial spatial unit, H2: two initial spatial units). It is intended to post-process the results of the \code{frag.simul.compare} function, but it can also be applied to other inputs.

The data frames with the results for H1 and H2 must have exactly the same column names, corresponding to parameters measured on the simulated graphs. When using the result generated with the \code{frag.simul.compare} function, the parameters considered include: the edge count, the sum of the edge weights, the balance, the components balance, the disturbance, the admixture and the cohesion of the two spatial units). When using this function alone, the names of the columns of the data frames can be set with the cohesion1.attr, cohesion2.attr, and admixture.attr parameters (default values:  "cohesion1", "cohesion2", "admixture" respectively).

For each parameter, a two-sample Wilcoxon test is run to compare the series of values generated for H1 and H2. In addition, the value measured on the observed graph is compared with the range of values generated for the two hypotheses. The results of these comparisons are reported as a data frame with four columns: for each parameter studied, the data frame contains 1) whether the series of H1 values are statistically different to the H2 series (Boolean), 2) the p-value of the Wilcoxon test (numerical), 3) whether the observed value is "within", "higher", or "lower" to the interquartile range of values for H1, 4) whether the observed value is "within", "higher", or "lower" to the interquartile range of values for H2.

}
\value{
A data frame summarising the comparison between the simulated results for the two hypotheses, and the values measured on the empirical graph with the simulated results.

}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
\code{\link[archeofrag]{frag.simul.compare}},
\code{\link[stats]{wilcox.test}},

}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
g <- frag.edges.weighting(g, layer.attr="layer")
\dontrun{res <- frag.simul.compare(g, layer.attr="layer", iter=30, summarise=FALSE)
frag.simul.summarise(g, layer.attr="layer", res.h1=res[[1]], res.h2=res[[2]])}
}
 
\keyword{simulation}
