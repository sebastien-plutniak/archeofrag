\name{make_frag_object}
\alias{make_frag_object}
\title{
    Makes a "frag.object" object.
}
\description{
    Makes a "\code{\link[=Frag.object-class]{frag.object}}" object.    
    
}
\usage{
make_frag_object(cr, sr, fragments)
}
\arguments{
  \item{cr}{
  A matrix or a data frame with two columns giving the vertex id of each pair of connected fragments.
}
  \item{sr}{
    Optional. A matrix or a data frame with two columns: the first gives the fragment id, the second gives the "similarity group" id. Optional if \code{mode} is "cr".
}
  \item{fragments}{
    A matrix or a data frame with information about each fragment. The first column must contain the fragments id. 
}

}
\details{This function checks the data-set and returns a "frag.object" which can be turned into a fragmentation graph using the \code{\link{make_cr_graph}}, \code{\link{make_sr_graph}}, or \code{\link{make_crsr_graph}} functions.}
\value{
    An object of "\code{\link[=Frag.object-class]{frag.object}}" class.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}
\examples{
cr.df <- matrix(c(1,2, 1,3, 2,3, 4,5, 4,6, 7,8), ncol=2, byrow=TRUE)
sr.df <- matrix( c(1,1, 9,1, 10,1, 11,2, 12,2, 13,2), ncol=2, byrow=TRUE)
fragments.df <- data.frame(1:13, letters[1:13]) 

make_frag_object(cr=cr.df, fragments=fragments.df)
make_frag_object(cr=cr.df, sr=sr.df, fragments=fragments.df)
}
\keyword{build data}

