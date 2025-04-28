\name{GrandeRivoire1stMeso}
\docType{data}

\alias{GrandeRivoire1stMeso}
\alias{grande.rivoire.1m.connection}
\alias{grande.rivoire.1m.fragments}

\title{Dataset: Refitting relationships between bone and lithic fragments from the Grande Rivoire site, First Mesolithic levels}
\description{
   Grande Rivoire is a Mesolithic rock shelter located in France. This dataset describes the refitting 'connection' relationships between bone and lithic fragments found during excavations of the First Mesolithic levels. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{grande.rivoire.1m.connection}: connection relationships between fragments.
        \item\code{grande.rivoire.1m.fragments}: information about the fragments.
    }
}
\usage{data(GrandeRivoire1stMeso)}
\format{
    \itemize{
        \item\code{grande.rivoire.1m.connection} is a 712x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{grande.rivoire.1m.fragments} is a 1165x5 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item layer: archaeological layer where the fragment was found
            \item layer.su: combination of the archaeological layer and sedimentary unit where the fragment was found
            \item sector: site sector where the fragment was found
            \item type: type of object (bone or lithic)
        }
    }
    
}
\references{
    \itemize{
		\item Derbord L.,  A. Angelin. 2025. 'Mesolithic artefact refitting data from La Grande Rivoire (Sassenage, Isère)'. Zenodo. https://doi.org/10.5281/zenodo.15289796
        }
    }
\keyword{datasets}

\examples{
data(GrandeRivoire1stMeso)
head(grande.rivoire.1m.fragments)
}

