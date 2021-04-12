\name{Frag.object-class}
\Rdversion{1.1}
\docType{class}
\alias{Frag.object-class}
\alias{make_cr_graph,Frag.object-method}
\alias{make_crsr_graph,Frag.object-method}
\alias{make_sr_graph,Frag.object-method}
\alias{show,Frag.object-method}

\title{Class \code{"Frag.object"}}
\description{
    A class for archaeological "fragmentation" dataset. This class construction aims to ensure that the data have been properly built before performing the next steps of the analysis.
    A convenient constructor function, \link{make_frag_object}, is provided.
    
}
\section{Objects from the Class}{
    Objects can be created by calls of the form:
    
    \code{make_frag_object(cr, sr, fragments)}.
}
\section{Slots}{
  \describe{
    \item{\code{cr.df}:}{Object of class \code{"matrix"} (\code{"data.frame"} are allowed and automatically converted)}
    \item{\code{sr.df}:}{Object of class \code{"matrix"}  (\code{"data.frame"} are allowed and automatically converted) }
    \item{\code{fragments.df}:}{Object of class \code{"data.frame"}}
    \item{\code{frag_type}:}{Object of class \code{"character"} }
  }
}
\section{Methods}{
  \describe{
    \item{make_cr_graph}{\code{signature(object = "Frag.object")}: 
    
    Makes an undirected graph representing the "connection" relations between archaeological fragments. Cf. \link{make_cr_graph} }
    \item{make_crsr_graph}{\code{signature(object = "Frag.object")}: 
    
    Makes an undirected graph combining the "connection" and "similarity" relations between archaeological fragments. Cf. \link{make_crsr_graph} }
    
    \item{show}{\code{signature(object = "Frag.object")}: 
    
    \code{show} method for Frag.object }
	 }
}

\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
showClass("Frag.object")
}
\keyword{build data}
