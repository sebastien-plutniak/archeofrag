\name{EtiollesD71}
\docType{data}

\alias{EtiollesD71}
\alias{etiollesD71.connection}
\alias{etiollesD71.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Etiolles site, dwellings D71}
\description{
   Etiolles is an Upper Palaeolithic open air site located in France. D71 refers to a set of three dwellings. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{etiollesD71.connection}: connection relationships between fragments.
        \item\code{etiollesD71.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(EtiollesD71)}
\format{
    \itemize{
        \item\code{etiollesD71.connection} is a 4999x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in columns 'id1' and 'id2', respectively. 
        \item\code{etiollesD71.fragments} is a 4143x7 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item layer: stratigraphic layer
            \item spit: excavation spits
            \item raw.material: type of archaeological material (flint, stone, bone, shell)
            \item debitage: lithic technology category
            \item x: x coordinate of the location where the lithic fragment was found (in metre)
            \item y: y coordinate of the location where the lithic fragment was found (in metre)
            \item z: z coordinate of the location where the lithic fragment was found (in metre)
        }
    }
    
}
\references{
    \itemize{
  		\item  Caron-Laviolette E. 2026. Lithic data from the D71 dwellings at Etiolles palaeolithic site (stone industries description and refitting), Zenodo, doi: https://doi.org/10.5281/zenodo.22663797.
        }
    }
\keyword{datasets}

\examples{
data(EtiollesD71)
head(etiollesD71.fragments)
comment(etiollesD71.fragments)
}

