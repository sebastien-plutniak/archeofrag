\name{Peyrazet}
\docType{data}

\alias{Peyrazet}
\alias{peyrazet.connection}
\alias{peyrazet.fragments}

\title{Dataset: Refitting relationships between lithic and bone fragments from the Peyrazet site}
\description{
   Peyrazet is an  Upper Paleolithic rock shelter site located in France. This dataset describes the refitting 'connection' relationships between lithic and bone fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{peyrazet.connection}: connection relationships between fragments.
        \item\code{peyrazet.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(Peyrazet)}
\format{
    \itemize{
        \item\code{peyrazet.connection} is a 234x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in columns 'id1' and 'id2', respectively. 
        \item\code{peyrazet.fragments} is a 234x7 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer: final stratigraphic layer assigned
            \item spit: excavation spits
            \item material: type of archaeological material (bone or lithic)
            \item x: x coordinate of the location where the lithic fragment was found (in cm)
            \item y: y coordinate of the location where the lithic fragment was found (in cm)
            \item z: z coordinate of the location where the lithic fragment was found (in cm)
        }
    }
    
}
\references{
    \itemize{
  		\item Langlais M., V. Laroulandie, S. Costamagno, J.-B. Mallye, J.-M. Pétillon, S. Rigaud. 2025. bdd_Peyrazet_Lot_France_fouilles2008_2015. Zenodo. https://doi.org/10.5281/zenodo.17870676.
        }
    }
\keyword{datasets}

\examples{
data(Peyrazet)
head(peyrazet.fragments)
comment(peyrazet.fragments)
}

