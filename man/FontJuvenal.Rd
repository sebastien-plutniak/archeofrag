\name{FontJuvenal}
\docType{data}

\alias{FontJuvenal}
\alias{fontjuvenal.connection}
\alias{fontjuvenal.fragments}

\title{Dataset: Refitting relationships between pottery fragments from Font-Juvenal cave}
\description{
   The Font-Juvenal cave is a Neolithic archaeological site in France. This dataset describes the refitting 'connection' relationships between pottery fragments found during excavations. A connection relationship refers to a physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{fontjuvenal.connection}: connection relationships between fragments.
        \item\code{fontjuvenal.fragments}: information about the fragments.
    }
}
\usage{data(FontJuvenal)}
\format{
    \itemize{
        \item\code{fontjuvenal.connection} is a 351x2 matrix. Each line describes a connection relationship between two fragments. Their  unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{fontjuvenal.fragments} is 354x4 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer:  simplified stratigraphic layer
            \item layer.detailed:  stratigraphic layer, with detail about where the fragment was found (in the sedimentary layer, or in a hearth contained in the layer)
            \item square: square where the fragment was found
        }
    }
    
}
\references{
    \itemize{
		\item  Caro J. 2024. 'Font-Juvénal_Refiting', Zenodo, doi:  https://doi.org/10.5281/zenodo.14515444     
        }
    }
\keyword{datasets}

\examples{
data(FontJuvenal)
head(fontjuvenal.fragments)
}

