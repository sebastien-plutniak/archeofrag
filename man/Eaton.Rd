\name{Eaton}
\docType{data}

\alias{Eaton}
\alias{eaton.connection}
\alias{eaton.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Eaton site}
\description{
   Eaton site is a 16th century Iroquoian village in western New York State, USA. This dataset describes the refitting 'connection' relationships between fragments of lithic points found during excavations led by William Engelbrecht. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{eaton.connection}: connection relationships between fragments.
        \item\code{eaton.fragments}: information about the fragments.
    }
}
\usage{data(Eaton)}
\format{
    \itemize{
        \item\code{eaton.connection} is a 1151x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{eaton.fragments} is 229x5 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item x: x label of the grid square where the lithic fragment was found (square size is 2m x 2m)
            \item y: y label of the grid square where the lithic fragment was found (square size is 2m x 2m)
            \item feature: archaeological feature associated with the fragment is associated ('long house', 'midden', or 'outside').
            \item level: archaeological level where the fragment was found (0 to 5, from top soil to bottom).
        }
    }
    
}
\references{
    \itemize{
		\item Engelbrecht W. 2014. "Madison Point Refits", tDAR, doi: 10.6067/xcv8t43v1j 
        \item Plutniak S. 2025. "Reprocessing script for William Engelbrecht's 'Madison Point Refits' dataset (including generated tables and figures)". Zenodo. doi: 10.5281/zenodo.15091301
        }
    }
\keyword{datasets}

\examples{
data(Eaton)
head(eaton.fragments)
}

