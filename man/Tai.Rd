\name{Tai}
\docType{data}

\alias{Tai}
\alias{tai.connection}
\alias{tai.fragments}

\title{Dataset: Refitting relationships between pottery fragments from the Tai site}
\description{
    The Tai is a Neolithic archaeological site in France. Three sectors were distinguished: the North entrance ('porche nord'), the South entrance ('porche sud') and the Cave ('grotte').  This dataset describes the refitting 'connection' relationships between pottery fragments found during excavations  by Claire Manen. A connection relationship refers to a physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{tai.connection}: 'connection' relationships between fragments.
        \item\code{tai.fragments}: information about the fragments.
    }
}
\usage{data(Tai)}
\format{
    \itemize{
        \item\code{tai.connection} is a 279x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{fontjuvenal.fragments} is 815x8 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer:   stratigraphic layer
            \item sector: sector of the site
            \item sherd_size: size class of the pottery sherd (nb: the numeric value is not metrical)
            \item sherd_thickness: thickness of the pottery sherd in millimeter
            \item x: x coordinates of the location where the pottery sherd was found
            \item y: y coordinates of the location where the pottery sherd was found
            \item z: z coordinates of the location where the pottery sherd was found
        }
    }
    
}
\references{
    \itemize{
		\item  Caro J., Plutniak S. 2022. “Refitting and Matching Neolithic Pottery Fragments from the Taï site, France”. doi: 10.5281/zenodo.7408706
        }
        
    }
\keyword{datasets}

\examples{
data(Tai)
head(tai.fragments)
}

