\name{frag.cohesion.ranking}
\alias{frag.cohesion.ranking}

\title{Rank a series of spatial units by comparing their cohesion values}
\description{Rank a series of spatial units based on how many times they feature the highest cohesion value in pairwise comparisons.}
\usage{frag.cohesion.ranking(cohesion.df, add.math.signs=TRUE)}

\arguments{
  \item{cohesion.df}{Matrix or data.frame, with two columns for the cohesion values for each pair of spatial units and spatial units labesl in row names.} 
  \item{add.math.signs}{Boolean. Wheter adding or not (in)equality  signs.}
}
\details{
Cohesion values computed with the  \code{frag.layers.cohesion()} are relative to the pair of spatial units under consideration and do not allow for comparison of more than two spatial units. The \code{frag.cohesion.ranking} function offers a method to analyse larger series of spatial units. 

Given the cohesion values computed for more than two spatial units, it counts how many times each spatial unit features the highest cohesion value in pairwise comparisons. Spatial units are sorted based on this count.

By default, the function's output is formated for human reading, adding equality and inequality signs in the series of spatial units labels to help in interpeting it.  This enhancement can be disabled with \code{add.math.signs=FALSE}.

The matrix or data frame given to the \code{cohesion.df} parameter must be formated as an output of the \code{frag.layers.cohesion()} function. Namely,  each row corresponds to the comparison of two spatial units, the two columns include the corresponding cohesion values, and the row name gives the labels of the spatial units (separated by the sign '/').
}
\value{
If  \code{add.math.signs=TRUE}, a data frame inclduding (in)equality signs and empty values. Else, a named numeric vector.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\seealso{
    \code{\link[archeofrag]{frag.layers.cohesion}}
}
\examples{
g1 <- frag.simul.process(n.components=5,  vertices=20, disturbance=.1)
g2 <- frag.simul.process(n.components=10, vertices=90, disturbance=.03)

# assign different names for these spatial units:
igraph::V(g2)$layer <- as.character(factor(igraph::V(g2)$layer, labels = c(3, 4))) 
igraph::V(g2)$name <- paste0(igraph::V(g2)$name, "g2")

g <- igraph::disjoint_union(g1, g2) # merge the graphs
g$frag_type <- "cr" # this value was lost while merging

cohesion.res <- frag.layers.cohesion(g, layer.attr="layer", verbose=FALSE) # compute cohesions
frag.cohesion.ranking(cohesion.res, add.math.signs = TRUE) # rank the spatial units
}
 
\keyword{measurement}
