\name{TaiCave}
\docType{data}

\alias{TaiCave}
\alias{tai.cave.connection}
\alias{tai.cave.fragments}

\title{Dataset: Refitting relationships between pottery fragments from the Tai site, Cave sector}
\description{
    The Tai is a Neolithic archaeological site in France. Three sectors were distinguished: the North entrance, the South entrance and the Cave. This dataset regards the refitting 'connection' relationships between pottery fragments found during excavations led by Claire Manen in the Cave sector. A connection relationship refers to a physical connection between two fragments that were part of the same object. 
    
    The dataset includes two tables:
    \itemize{
        \item\code{tai.cave.connection}: connection relationships between fragments.
        \item\code{tai.cave.fragments}: information about the fragments.
    }
}
\usage{data(TaiCave)}
\format{
    \itemize{
        \item\code{tai.cave.connection} is a 91x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{tai.cave.fragments} is 112x7 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer:   stratigraphic layer
            \item sherd_size: size class of the pottery sherd (nb: the numeric value is not metrical)
            \item sherd_thickness: thickness of the pottery sherd in millimeter
            \item x: x coordinates of the location where the pottery sherd was found (in centimetres)
            \item y: y coordinates of the location where the pottery sherd was found (in centimetres)
            \item z: z coordinates of the location where the pottery sherd was found (in centimetres)
        }
    }
    
}
\references{
    \itemize{
		\item  Caro J., Plutniak S. 2022. 'Refitting and Matching Neolithic Pottery Fragments from the Taï site, France'. Zenodo, doi: https://doi.org/10.5281/zenodo.7408706
        }
        
    }
\keyword{datasets}

\examples{
data(TaiCave)
head(tai.cave.fragments)
}

