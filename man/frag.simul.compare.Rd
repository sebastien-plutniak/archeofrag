\name{frag.simul.compare}
\alias{frag.simul.compare}

\title{From an observed fragmentation graph, simulates two series of graphs corresponding to two deposition hypotheses.}
\description{Given an observed fragmentation graph, simulates two series of graphs corresponding to two deposition hypotheses, compares their properties and returns a summary table.}
\usage{frag.simul.compare(graph, layer.attr, iter, summarise=TRUE, verbose=TRUE, ...)}

\arguments{
  \item{graph}{An undirected \code{igraph} object. The 'observed' graph to compare to simulated graphs.} 
  \item{layer.attr}{Character. The name of the vertices attribute giving the layer of the fragments.}
  \item{iter}{Numerical. The number of simulated graphs to generate for each hypothesis (minimal value: 30).}
  \item{summarise}{Logical. Whether to report a comparative summary of the results.}
  \item{verbose}{Logical. Whether to print or not warning messages.}
  \item{...}{Further arguments passed to the `frag.simul.process` function.}
}
\details{
This function is a convenient wrapper integrating several functions of the \code{archeofrag} package to compare an observed fragmentation graph to similar simulated graphs. The \code{frag.simul.process} is used to generate two series of graphs from the properties of the observed graph: the first series is generated under the formation hypothesis H1 (one initial spatial unit) and the second series is generated under the hypothesis H2 (two initial spatial units). 
The edge count, edge weights sum, balance, components balance, disturbance, admixture, and cohesion values of the generated graphs are measured.

By default, the results are post-processed with the \code{frag.simul.summarise} function and a summary data frame is printed and included in the list of results which is silently returned. If the \code{summarise} parameter is set to FALSE, then the function returns a list of two data frames containing the numeric values measured for H1 and H2.


}
\value{
A named list with three items: 
"h1.data", a data frame with the numerical values measured on the graphs generated for H1;
"h2.data", a data frame with the numerical values measured on the graphs generated for H2;
"summary", a data frame summarising the comparison between the results for the two hypotheses and the values measured on the empirical graph.

}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
\code{\link[archeofrag]{frag.simul.process}},
\code{\link[archeofrag]{frag.simul.summarise}}
}
\examples{
g <- frag.simul.process(n.components=20, vertices=50, disturbance=.15)
\dontrun{frag.simul.compare(g, layer.attr="layer", iter=30)}
}
 
\keyword{simulation}
