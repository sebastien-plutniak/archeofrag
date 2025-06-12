\name{Fumane}
\docType{data}

\alias{Fumane}
\alias{fumane.connection}
\alias{fumane.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Fumane cave}
\description{
   The Fumane Cave is a Paleolithic archaeological site in Italy. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. A connection relationship refers to a physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{fumane.connection}: connection relationships between fragments.
        \item\code{fumane.fragments}: information about the fragments.
    }
}
\usage{data(Fumane)}
\format{
    \itemize{
        \item\code{fumane.connection} is a 261x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{fumane.fragments} is 508x8 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item us: stratigraphic layer
            \item square:  excavation grid square where the fragment was found
            \item sub_square:  excavation grid subsquare where the fragment was found
            \item thickness: thickness of the fragment (in millimeter)
            \item length: length of the fragment (in millimeter)
            \item width: width of the fragment (in millimeter)
            \item volume: approximated volume of the fragment, product of the thickness by the length by the width (in cubic millimeter)
        }
    }
    
}
\references{
    \itemize{
		\item  Armando Falcucci. 2025. Refitting-The-Context: Accepted paper b (v0.1.3). Zenodo. https://doi.org/10.5281/zenodo.15017627  
        }
    }
\keyword{datasets}

\examples{
data(Fumane)
head(fumane.fragments)
}

